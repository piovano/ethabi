#![recursion_limit="256"]

extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;
#[macro_use]
extern crate quote;
extern crate heck;
extern crate ethabi;

mod constructor;
mod contract;
mod event;
mod function;

use std::{env, fs};
use std::path::PathBuf;
use heck::SnakeCase;
use syn::export::Span;
use ethabi::{Result, ResultExt, Contract, Param, ParamType};

const ERROR_MSG: &'static str = "`derive(EthabiContract)` failed";

#[proc_macro_derive(EthabiContract, attributes(ethabi_contract_options))]
pub fn ethabi_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let ast = syn::parse(input).expect(ERROR_MSG);
	let gen = impl_ethabi_derive(&ast).expect(ERROR_MSG);
	gen.into()
}

fn impl_ethabi_derive(ast: &syn::DeriveInput) -> Result<proc_macro2::TokenStream> {
	let options = get_options(&ast.attrs, "ethabi_contract_options")?;
	let path = get_option(&options, "path")?;
	let normalized_path = normalize_path(&path)?;
	let source_file = fs::File::open(&normalized_path)
		.chain_err(|| format!("Cannot load contract abi from `{}`", normalized_path.display()))?;
	let contract = Contract::load(source_file)?;
	let c = contract::Contract::from(&contract);
	Ok(c.generate())
}

fn get_options(attrs: &[syn::Attribute], name: &str) -> Result<Vec<syn::NestedMeta>> {
	let options = attrs.iter()
		.flat_map(syn::Attribute::interpret_meta)
		.find(|meta| meta.name() == name);


	match options {
		Some(syn::Meta::List(list)) => Ok(list.nested.into_iter().collect()),
		_ => Err("Unexpected meta item".into())
	}
}

fn get_option(options: &[syn::NestedMeta], name: &str) -> Result<String> {
	let item = options.iter()
		.flat_map(|nested| match *nested {
			syn::NestedMeta::Meta(ref meta) => Some(meta),
			_ => None,
		})
		.find(|meta| meta.name() == name)
		.chain_err(|| format!("Expected to find option {}", name))?;
	str_value_of_meta_item(item, name)
}

fn str_value_of_meta_item(item: &syn::Meta, name: &str) -> Result<String> {
	if let syn::Meta::NameValue(ref name_value) = *item {
		if let syn::Lit::Str(ref value) = name_value.lit {
			return Ok(value.value());
		}
	}

	Err(format!(r#"`{}` must be in the form `#[{}="something"]`"#, name, name).into())
}

fn normalize_path(relative_path: &str) -> Result<PathBuf> {
	// workaround for https://github.com/rust-lang/rust/issues/43860
	let cargo_toml_directory = env::var("CARGO_MANIFEST_DIR").chain_err(|| "Cannot find manifest file")?;
	let mut path: PathBuf = cargo_toml_directory.into();
	path.push(relative_path);
	Ok(path)
}

fn to_syntax_string(param_type: &ethabi::ParamType) -> proc_macro2::TokenStream {
	match *param_type {
		ParamType::Address => quote! { ethabi::ParamType::Address },
		ParamType::Bytes => quote! { ethabi::ParamType::Bytes },
		ParamType::Int(x) => quote! { ethabi::ParamType::Int(#x) },
		ParamType::Uint(x) => quote! { ethabi::ParamType::Uint(#x) },
		ParamType::Bool => quote! { ethabi::ParamType::Bool },
		ParamType::String => quote! { ethabi::ParamType::String },
		ParamType::Array(ref param_type) => {
			let param_type_quote = to_syntax_string(param_type);
			quote! { ethabi::ParamType::Array(Box::new(#param_type_quote)) }
		},
		ParamType::FixedBytes(x) => quote! { ethabi::ParamType::FixedBytes(#x) },
		ParamType::FixedArray(ref param_type, ref x) => {
			let param_type_quote = to_syntax_string(param_type);
			quote! { ethabi::ParamType::FixedArray(Box::new(#param_type_quote), #x) }
		},
		ParamType::Tuple(ref param_types) => {
			let param_type_quotes = param_types.iter().map(|t| to_syntax_string(t)).collect::<Vec<_>>();
			quote! { ethabi::ParamType::Tuple(vec![ #(#param_type_quotes),* ]) }
		}
	}
}

fn to_ethabi_param_vec<'a, P: 'a>(params: P) -> proc_macro2::TokenStream
	where P: IntoIterator<Item = &'a Param>
{
	fn to_ethabi_param(param: &Param) -> proc_macro2::TokenStream {
		let name = &param.name;
		let kind = to_syntax_string(&param.kind);
		let components = if let Some(ref components) = param.components {
			let components = components.iter().map(|component| {
				to_ethabi_param(component)
			}).collect::<Vec<_>>();
			quote! { Some(vec![ #(#components),* ]) }
		} else {
			quote! { None }
		};
		quote! {
			ethabi::Param {
				name: #name.to_owned(),
				kind: #kind,
				components: #components,
			}
		}
	}

	let p = params.into_iter().map(|x| {
		to_ethabi_param(x)
	}).collect::<Vec<_>>();

	quote! { vec![ #(#p),* ] }
}

fn rust_type(input: &ParamType) -> proc_macro2::TokenStream {
	match *input {
		ParamType::Address => quote! { ethabi::Address },
		ParamType::Bytes => quote! { ethabi::Bytes },
		ParamType::FixedBytes(32) => quote! { ethabi::Hash },
		ParamType::FixedBytes(size) => quote! { [u8; #size] },
		ParamType::Int(_) => quote! { ethabi::Int },
		ParamType::Uint(_) => quote! { ethabi::Uint },
		ParamType::Bool => quote! { bool },
		ParamType::String => quote! { String },
		ParamType::Array(ref kind) => {
			let t = rust_type(&*kind);
			quote! { Vec<#t> }
		},
		ParamType::FixedArray(ref kind, size) => {
			let t = rust_type(&*kind);
			quote! { [#t, #size] }
		},
		ParamType::Tuple(ref kinds) => {
			let ts = kinds.iter().map(|k| rust_type(&*k)).collect::<Vec<_>>();
			quote! { (#(#ts),*) }
		}
	}
}

fn _template_param_type(input: &ParamType, prefix: &str, index: usize) -> proc_macro2::TokenStream {
	let t_ident = syn::Ident::new(&format!("T{}{}", prefix, index), Span::call_site());
	let u_ident = syn::Ident::new(&format!("U{}{}", prefix, index), Span::call_site());
	match *input {
		ParamType::Address => quote! { #t_ident: Into<ethabi::Address> },
		ParamType::Bytes => quote! { #t_ident: Into<ethabi::Bytes> },
		ParamType::FixedBytes(32) => quote! { #t_ident: Into<ethabi::Hash> },
		ParamType::FixedBytes(size) => quote! { #t_ident: Into<[u8; #size]> },
		ParamType::Int(_) => quote! { #t_ident: Into<ethabi::Int> },
		ParamType::Uint(_) => quote! { #t_ident: Into<ethabi::Uint> },
		ParamType::Bool => quote! { #t_ident: Into<bool> },
		ParamType::String => quote! { #t_ident: Into<String> },
		ParamType::Array(ref kind) => {
			let t = rust_type(&*kind);
			quote! {
				#t_ident: IntoIterator<Item = #u_ident>, #u_ident: Into<#t>
			}
		},
		ParamType::FixedArray(ref kind, size) => {
			let t = rust_type(&*kind);
			quote! {
				#t_ident: Into<[#u_ident; #size]>, #u_ident: Into<#t>
			}
		},
		ParamType::Tuple(ref kinds) => {
			let sub_tokens = kinds.iter().enumerate().map(|(i, k)| {
				_template_param_type(k, &format!("{}{}_", prefix, index), i)
			}).collect::<Vec<_>>();

			quote! {
				#(#sub_tokens),*
			}
		}
	}
}

fn template_param_type(input: &ParamType, index: usize) -> proc_macro2::TokenStream {
	_template_param_type(input, "", index)
}

fn _template_param_type_name(input: &ParamType, prefix: &str, index: usize) -> proc_macro2::TokenStream {
	match *input {
		ParamType::Tuple(ref kinds) => {
			let sub_tokens = kinds.iter().enumerate().map(|(i, k)| {
				_template_param_type_name(k, &format!("{}{}_", prefix, index), i)
			}).collect::<Vec<_>>();

			quote! {
				( #(#sub_tokens),* )
			}
		}
		_ => {
			let t_ident = syn::Ident::new(&format!("T{}{}", prefix, index), Span::call_site());
			quote! { #t_ident }
		}
	}
}

fn template_param_type_name(input: &ParamType, index: usize) -> proc_macro2::TokenStream {
	_template_param_type_name(input, "", index)
}

fn from_template_param(input: &ParamType, name: &proc_macro2::TokenStream) -> proc_macro2::TokenStream {
	match *input {
		ParamType::Array(_) => quote! { #name.into_iter().map(Into::into).collect::<Vec<_>>() },
		ParamType::FixedArray(_, _) => quote! { (Box::new(#name.into()) as Box<[_]>).into_vec().into_iter().map(Into::into).collect::<Vec<_>>() },
		ParamType::Tuple(_) => quote! { #name },
		_ => quote! { #name.into() },
	}
}

fn to_token(name: &proc_macro2::TokenStream, kind: &ParamType) -> proc_macro2::TokenStream {
	match *kind {
		ParamType::Address => quote! { ethabi::Token::Address(#name) },
		ParamType::Bytes => quote! { ethabi::Token::Bytes(#name) },
		ParamType::FixedBytes(_) => quote! { ethabi::Token::FixedBytes(#name.to_vec()) },
		ParamType::Int(_) => quote! { ethabi::Token::Int(#name) },
		ParamType::Uint(_) => quote! { ethabi::Token::Uint(#name) },
		ParamType::Bool => quote! { ethabi::Token::Bool(#name) },
		ParamType::String => quote! { ethabi::Token::String(#name) },
		ParamType::Array(ref kind) => {
			let inner_name = quote! { inner };
			let inner_loop = to_token(&inner_name, kind);
			quote! {
				// note the double {{
				{
					let v = #name.into_iter().map(|#inner_name| #inner_loop).collect();
					ethabi::Token::Array(v)
				}
			}
		}
		ParamType::FixedArray(ref kind, _) => {
			let inner_name = quote! { inner };
			let inner_loop = to_token(&inner_name, kind);
			quote! {
				// note the double {{
				{
					let v = #name.into_iter().map(|#inner_name| #inner_loop).collect();
					ethabi::Token::FixedArray(v)
				}
			}
		},
		ParamType::Tuple(ref kinds) => {
			let ts = kinds.iter().enumerate().map(|(i, k)| {
				let i_literal = proc_macro2::Literal::usize_unsuffixed(i);
				let inner_name = from_template_param(k, &quote! { #name.#i_literal });
				to_token(&inner_name, k)
			}).collect::<Vec<_>>();
			quote! {
				// note the double {{
				{
					let v = vec![ #(#ts),* ];
					ethabi::Token::Tuple(v)
				}
			}
		}
	}
}

fn from_token(kind: &ParamType, token: &proc_macro2::TokenStream) -> proc_macro2::TokenStream {
	match *kind {
		ParamType::Address => quote! { #token.to_address().expect(INTERNAL_ERR) },
		ParamType::Bytes => quote! { #token.to_bytes().expect(INTERNAL_ERR) },
		ParamType::FixedBytes(32) => quote! {
			{
				let mut result = [0u8; 32];
				let v = #token.to_fixed_bytes().expect(INTERNAL_ERR);
				result.copy_from_slice(&v);
				ethabi::Hash::from(result)
			}
		},
		ParamType::FixedBytes(size) => {
			let size: syn::Index = size.into();
			quote! {
				{
					let mut result = [0u8; #size];
					let v = #token.to_fixed_bytes().expect(INTERNAL_ERR);
					result.copy_from_slice(&v);
					result
				}
			}
		},
		ParamType::Int(_) => quote! { #token.to_int().expect(INTERNAL_ERR) },
		ParamType::Uint(_) => quote! { #token.to_uint().expect(INTERNAL_ERR) },
		ParamType::Bool => quote! { #token.to_bool().expect(INTERNAL_ERR) },
		ParamType::String => quote! { #token.to_string().expect(INTERNAL_ERR) },
		ParamType::Array(ref kind) => {
			let inner = quote! { inner };
			let inner_loop = from_token(kind, &inner);
			quote! {
				#token.to_array().expect(INTERNAL_ERR).into_iter()
					.map(|#inner| #inner_loop)
					.collect()
			}
		},
		ParamType::FixedArray(ref kind, size) => {
			let inner = quote! { inner };
			let inner_loop = from_token(kind, &inner);
			let to_array = vec![quote! { iter.next() }; size];
			quote! {
				{
					let iter = #token.to_array().expect(INTERNAL_ERR).into_iter()
						.map(|#inner| #inner_loop);
					[#(#to_array),*]
				}
			}
		},
		ParamType::Tuple(ref kinds) => {
			let inner_name = quote! { iter.next().expect(INTERNAL_ERR) };
			let ts = kinds.iter().map(|k| {
				from_token(k, &inner_name)
			}).collect::<Vec<_>>();
			quote! {
				{
					let mut iter = #token.to_tuple().expect(INTERNAL_ERR).into_iter();
					(#(#ts),*)
				}
			}
		}
	}
}

fn input_names(inputs: &Vec<Param>) -> Vec<proc_macro2::TokenStream> {
	inputs
		.iter()
		.enumerate()
		.map(|(index, param)| {
			let ident = if param.name.is_empty() {
				syn::Ident::new(&format!("param{}", index), Span::call_site())
			} else {
				syn::Ident::new(&rust_variable(&param.name), Span::call_site())
			};
			quote! { #ident }
		})
		.collect()
}

fn get_output_kinds(outputs: &Vec<Param>) -> proc_macro2::TokenStream {
	match outputs.len() {
		0 => quote! {()},
		1 => {
			let t = rust_type(&outputs[0].kind);
			quote! { #t }
		},
		_ => {
			let outs: Vec<_> = outputs
				.iter()
				.map(|param| rust_type(&param.kind))
				.collect();
			quote! { (#(#outs),*) }
		}
	}
}

/// Convert input into a rust variable name.
///
/// Avoid using keywords by escaping them.
fn rust_variable(name: &str) -> String {
	// avoid keyword parameters
	match name {
		"self" => "_self".to_string(),
		other => other.to_snake_case(),
	}
}
