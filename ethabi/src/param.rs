//! Function param.
use std::fmt;
use serde::de::{self, Deserialize, Deserializer, Visitor, MapAccess};
use ParamType;

/// Function param.
#[derive(Debug, Clone, PartialEq)]
pub struct Param {
	/// Param name.
	pub name: String,
	/// Param type.
	pub kind: ParamType,
	/// Param components.
	pub components: Option<Vec<Param>>,
}

impl<'de> Deserialize<'de> for Param {
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: Deserializer<'de> {
		#[derive(Deserialize)]
		#[serde(field_identifier, rename_all = "lowercase")]
		enum Field { Name, Type, Components }

		struct ParamVisitor;

		impl<'de> Visitor<'de> for ParamVisitor {
			type Value = Param;

			fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
				formatter.write_str("valid abi spec param")
			}

			fn visit_map<V>(self, mut map: V) -> Result<Param, V::Error> where V: MapAccess<'de> {
				let mut name = None;
				let mut kind = None;
				let mut components: Option<Vec<Param>> = None;
				while let Some(key) = map.next_key()? {
					match key {
						Field::Name => {
							if name.is_some() {
								return Err(de::Error::duplicate_field("name"));
							}
							name = Some(map.next_value()?);
						},
						Field::Type => {
							if kind.is_some() {
								return Err(de::Error::duplicate_field("type"));
							}
							kind = Some(map.next_value()?);
						},
						Field::Components => {
							if components.is_some() {
								return Err(de::Error::duplicate_field("components"));
							}
							components = Some(map.next_value()?);
						},
					}
				}
				let name = name.ok_or_else(|| de::Error::missing_field("name"))?;
				let mut kind = kind.ok_or_else(|| de::Error::missing_field("type"))?;
				if let ParamType::Tuple(ref mut params) = kind {
					if let Some(ref components) = components {
						*params = components.iter().map(|component| component.kind.clone()).collect::<Vec<_>>();
					}
				}
				Ok(Param { name, kind, components })
			}
		}

		const FIELDS: &'static [&'static str] = &["name", "type", "components"];
		deserializer.deserialize_struct("Param", FIELDS, ParamVisitor)
	}
}

#[cfg(test)]
mod tests {
	use serde_json;
	use {Param, ParamType};

	#[test]
	fn param_deserialization() {
		let s = r#"{
			"name": "foo",
			"type": "address"
		}"#;

		let deserialized: Param = serde_json::from_str(s).unwrap();

		assert_eq!(deserialized, Param {
			name: "foo".to_owned(),
			kind: ParamType::Address,
			components: None,
		});
	}

	#[test]
	fn param_tuple_deserialization() {
		let s = r#"{
			"components": [
				{
					"name": "s",
					"type": "string"
				},
				{
					"name": "b",
					"type": "bool[]"
				}
			],
			"name": "foo",
			"type": "tuple"
		}"#;

		let deserialized: Param = serde_json::from_str(s).unwrap();

		assert_eq!(deserialized, Param {
			name: "foo".to_owned(),
			kind: ParamType::Tuple(vec![
				ParamType::String,
				ParamType::Array(Box::new(ParamType::Bool)),
			]),
			components: Some(vec![
				Param {
					name: "s".to_owned(),
					kind: ParamType::String,
					components: None,
				},
				Param {
					name: "b".to_owned(),
					kind: ParamType::Array(Box::new(ParamType::Bool)),
					components: None,
				}
			]),
		});
	}
}
