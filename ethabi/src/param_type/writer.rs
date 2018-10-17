use std::cell::RefCell;
use std::fmt;
use ParamType;

/// Output formatter for param type.
pub struct Writer;

impl Writer {
	/// Returns string which is a formatted represenation of param.
	pub fn write(param: &ParamType) -> String {
		match *param {
			ParamType::Address => "address".to_owned(),
			ParamType::Bytes => "bytes".to_owned(),
			ParamType::FixedBytes(len) => format!("bytes{}", len),
			ParamType::Int(len) => format!("int{}", len),
			ParamType::Uint(len) => format!("uint{}", len),
			ParamType::Bool => "bool".to_owned(),
			ParamType::String => "string".to_owned(),
			ParamType::FixedArray(ref param, len) => format!("{}[{}]", Writer::write(param), len),
			ParamType::Array(ref param) => format!("{}[]", Writer::write(param)),
			ParamType::Tuple(ref params) => format!("({})", TupleParams::new(&params)),
		}
	}
}

struct TupleParams<'a>(RefCell<Option<::std::slice::Iter<'a, ParamType>>>);

impl<'a> fmt::Display for TupleParams<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let mut iter = match self.0.borrow_mut().take() {
			Some(t) => t,
			None => panic!("Format: was already formatted once"),
		};
		if let Some(first) = iter.next() {
			try!(fmt::Display::fmt(&first, f));
			for param in iter {
				try!(f.write_str(","));
				try!(fmt::Display::fmt(&param, f));
			}
		}
		Ok(())
	}
}

impl<'a> TupleParams<'a> {
	fn new(params: &'a Vec<ParamType>) -> Self {
		TupleParams(RefCell::new(Some(params.iter())))
	}
}

#[cfg(test)]
mod tests {
	use ParamType;
	use super::Writer;

	#[test]
	fn test_write_param() {
		assert_eq!(Writer::write(&ParamType::Address), "address".to_owned());
		assert_eq!(Writer::write(&ParamType::Bytes), "bytes".to_owned());
		assert_eq!(Writer::write(&ParamType::FixedBytes(32)), "bytes32".to_owned());
		assert_eq!(Writer::write(&ParamType::Uint(256)), "uint256".to_owned());
		assert_eq!(Writer::write(&ParamType::Int(64)), "int64".to_owned());
		assert_eq!(Writer::write(&ParamType::Bool), "bool".to_owned());
		assert_eq!(Writer::write(&ParamType::String), "string".to_owned());
		assert_eq!(Writer::write(&ParamType::Array(Box::new(ParamType::Bool))), "bool[]".to_owned());
		assert_eq!(Writer::write(&ParamType::FixedArray(Box::new(ParamType::String), 2)), "string[2]".to_owned());
		assert_eq!(Writer::write(&ParamType::FixedArray(Box::new(ParamType::Array(Box::new(ParamType::Bool))), 2)), "bool[][2]".to_owned());
		assert_eq!(Writer::write(&ParamType::Tuple(vec![ParamType::Bool, ParamType::Tuple(vec![ParamType::Address, ParamType::String])])), "(bool,(address,string))".to_owned())
	}
}
