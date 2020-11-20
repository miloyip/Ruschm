use either::Either;

use std::{fmt::Display, iter::FromIterator};

// use super::error::error;

// r7rs 6.4. Pairs and lists

pub trait Pairable {
    fn either_pair_mut(&mut self) -> Either<&mut GenericPair<Self>, &mut Self>
    where
        Self: From<Box<GenericPair<Self>>>;

    fn either_pair_ref(&self) -> Either<&GenericPair<Self>, &Self>
    where
        Self: From<Box<GenericPair<Self>>>;

    fn into_pair(self) -> Either<GenericPair<Self>, Self>
    where
        Self: From<Box<GenericPair<Self>>>;
}

#[derive(Debug, Clone, PartialEq)]
pub enum GenericPair<T>
where
    T: From<Box<GenericPair<T>>> + Pairable,
{
    Some(T, T),
    Empty,
}

impl<T: From<Box<GenericPair<T>>> + Pairable + Display> Display for GenericPair<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        let mut current_value = self;
        loop {
            match current_value {
                GenericPair::Some(car, cdr) => {
                    write!(f, "{}", car)?;
                    match cdr.either_pair_ref() {
                        Either::Left(GenericPair::Empty) => (),
                        Either::Left(pair) => {
                            write!(f, " ")?;
                            current_value = pair;
                        }
                        other => {
                            write!(f, " {}", other)?;
                            break;
                        }
                    }
                }
                GenericPair::Empty => break,
            };
        }
        write!(f, ")")
    }
}

// collect as a list, return head node
impl<T: From<Box<GenericPair<T>>> + Pairable> FromIterator<T> for Box<GenericPair<T>> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut head = Box::new(GenericPair::Empty);
        let mut tail = head.as_mut();
        for element in iter {
            match tail {
                GenericPair::Empty => {
                    head = Box::new(GenericPair::Some(
                        element,
                        T::from(Box::new(GenericPair::Empty)),
                    ));
                    tail = &mut head;
                }
                GenericPair::Some(_, cdr) => {
                    // tail = Box::into_raw();
                    *cdr = T::from(Box::new(GenericPair::Some(
                        element,
                        T::from(Box::new(GenericPair::Empty)),
                    )));
                    tail = cdr.either_pair_mut().left().unwrap();
                }
            }
        }
        head
    }
}

impl<T: From<Box<GenericPair<T>>> + Pairable> IntoIterator for GenericPair<T> {
    type Item = T;
    type IntoIter = GenericPairIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        GenericPairIter {
            next: Some(T::from(Box::new(self))),
        }
    }
}

pub struct GenericPairIter<T>
where
    T: From<Box<GenericPair<T>>> + Pairable,
{
    next: Option<T>,
}

impl<T: From<Box<GenericPair<T>>> + Pairable> Iterator for GenericPairIter<T> {
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.next.take();
        match next {
            Some(next) => match next.into_pair() {
                Either::Left(GenericPair::Some(car, cdr)) => {
                    // let current = Some(*car);
                    self.next = Some(cdr);
                    Some(car)
                }
                Either::Left(GenericPair::Empty) => None,
                Either::Right(next) => {
                    self.next = Some(T::from(Box::new(GenericPair::Empty)));
                    Some(next)
                }
            },
            None => None,
        }
    }

    type Item = T;
}

#[cfg(test)]
macro_rules! list {
    ($($x:expr),*) => {
        vec![$($x)*].into_iter().collect::<Box<GenericPair<_>>>()
    };
}
