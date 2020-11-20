use std::fmt::Display;

use either::Either;

use crate::error::*;

use super::{
    error::SyntaxError,
    pair::{GenericPair, Pairable},
};

#[derive(PartialEq, Debug, Clone)]
pub enum Primitive {
    String(String),
    Character(char),
    Boolean(bool),
    Integer(i32),
    Rational(i32, u32),
    Real(String),
}

impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Primitive::String(inner) => write!(f, "{}", inner),
            Primitive::Character(inner) => write!(f, "{}", inner),
            Primitive::Boolean(inner) => write!(f, "{}", if *inner { "#t" } else { "#f" }),
            Primitive::Integer(inner) => write!(f, "{}", inner),
            Primitive::Rational(a, b) => write!(f, "{}/{}", a, b),
            Primitive::Real(inner) => write!(f, "{}", inner),
        }
    }
}

pub type DatumPair = GenericPair<Datum>;

#[derive(PartialEq, Debug, Clone)]
pub enum DatumBody {
    Primitive(Primitive),
    Symbol(String),
    List(Box<DatumPair>),
    Vector(Vec<Datum>),
}

impl Display for Datum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.data {
            DatumBody::Primitive(inner) => write!(f, "{}", inner),
            DatumBody::Symbol(inner) => write!(f, "{}", inner),
            DatumBody::List(inner) => write!(f, "{}", inner),
            DatumBody::Vector(_) => todo!(),
        }
    }
}

impl Datum {
    pub fn expect_list(self) -> Result<DatumPair, SchemeError> {
        self.into_pair().left().ok_or(
            ErrorData::from(SyntaxError::ExpectSomething("list/pair".to_string())).no_locate(),
        )
    }
}

impl Pairable for Datum {
    fn either_pair_mut(&mut self) -> Either<&mut DatumPair, &mut Self> {
        match self {
            Datum {
                data: DatumBody::List(pair),
                ..
            } => Either::Left(pair.as_mut()),
            _ => Either::Right(self),
        }
    }
    fn either_pair_ref(&self) -> Either<&DatumPair, &Self> {
        match self {
            Datum {
                data: DatumBody::List(pair),
                ..
            } => Either::Left(pair.as_ref()),
            _ => Either::Right(self),
        }
    }
    fn into_pair(self) -> Either<DatumPair, Self> {
        match self {
            Datum {
                data: DatumBody::List(pair),
                ..
            } => Either::Left(*pair),
            _ => Either::Right(self),
        }
    }
}

impl From<Box<DatumPair>> for Datum {
    fn from(pair: Box<DatumPair>) -> Self {
        Datum {
            data: DatumBody::List(pair),
            location: None, // TODO
        }
    }
}

pub type Datum = Located<DatumBody>;

impl ToLocated for DatumBody {}
