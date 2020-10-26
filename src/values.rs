use std::{
    cell::RefCell,
    cell::RefMut,
    cmp::Ordering,
    fmt::{self, Debug, Display, Formatter},
    ops::Deref,
    rc::Rc,
};

use itertools::join;
use num_traits::real::Real;
use smallvec::SmallVec;

use crate::{
    environment::*, error::*, interpreter::pair::Pair, parser::ParameterFormals,
    parser::SchemeProcedure,
};

type Result<T> = std::result::Result<T, SchemeError>;

pub trait RealNumberInternalTrait: Display + Debug + Real
where
    Self: std::marker::Sized,
{
}

impl<T: Display + Debug + Real> RealNumberInternalTrait for T {}
#[derive(Debug, Clone, Copy)]
pub enum Number<R: RealNumberInternalTrait> {
    Integer(i32),
    Real(R),
    Rational(i32, i32),
}

impl<R: RealNumberInternalTrait> Display for Number<R> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Number::Integer(n) => write!(f, "{}", n),
            Number::Real(n) => write!(f, "{:?}", n),
            Number::Rational(a, b) => write!(f, "{}/{}", a, b),
        }
    }
}

impl<R: RealNumberInternalTrait> Number<R> {
    pub(crate) fn exact_eqv(&self, other: &Self) -> bool {
        match (self, other) {
            (Number::Integer(a), Number::Integer(b)) => a.eq(&b),
            (Number::Rational(a1, b1), Number::Rational(a2, b2)) => (a1 * b2).eq(&(b1 * a2)),
            (Number::Real(a), Number::Real(b)) => a.eq(&b),
            _ => false,
        }
    }
}

// in the sense of '=', not eq?, eqv?, nor equal?
impl<R: RealNumberInternalTrait> PartialEq for Number<R> {
    fn eq(&self, other: &Number<R>) -> bool {
        match upcast_oprands((*self, *other)) {
            NumberBinaryOperand::Integer(a, b) => a.eq(&b),
            NumberBinaryOperand::Rational(a1, a2, b1, b2) => (a1 * b2).eq(&(b1 * a2)),
            NumberBinaryOperand::Real(a, b) => a.eq(&b),
        }
    }
}

impl<R: RealNumberInternalTrait> PartialOrd for Number<R> {
    fn partial_cmp(&self, other: &Number<R>) -> Option<Ordering> {
        match upcast_oprands((*self, *other)) {
            NumberBinaryOperand::Integer(a, b) => a.partial_cmp(&b),
            NumberBinaryOperand::Rational(a1, a2, b1, b2) => (a1 * b2).partial_cmp(&(b1 * a2)),
            NumberBinaryOperand::Real(a, b) => a.partial_cmp(&b),
        }
    }
}

pub(crate) enum NumberBinaryOperand<R: RealNumberInternalTrait> {
    Integer(i32, i32),
    Real(R, R),
    Rational(i32, i32, i32, i32),
}

// Integer => Rational => Real
pub(crate) fn upcast_oprands<R: RealNumberInternalTrait>(
    operand: (Number<R>, Number<R>),
) -> NumberBinaryOperand<R> {
    match operand {
        (Number::Rational(dividend, dividor), Number::Real(b)) => {
            NumberBinaryOperand::Real(R::from(dividend).unwrap() / R::from(dividor).unwrap(), b)
        }
        (Number::Real(a), Number::Rational(dividend, dividor)) => {
            NumberBinaryOperand::Real(a, R::from(dividend).unwrap() / R::from(dividor).unwrap())
        }
        (Number::Integer(a), Number::Real(b)) => NumberBinaryOperand::Real(R::from(a).unwrap(), b),
        (Number::Real(a), Number::Integer(b)) => NumberBinaryOperand::Real(a, R::from(b).unwrap()),
        (Number::Rational(dividend, dividor), Number::Integer(b)) => {
            NumberBinaryOperand::Rational(dividend, dividor, b, 1)
        }
        (Number::Integer(a), Number::Rational(dividend, dividor)) => {
            NumberBinaryOperand::Rational(a, 1, dividend, dividor)
        }
        (Number::Integer(a), Number::Integer(b)) => (NumberBinaryOperand::Integer(a, b)),
        (Number::Real(a), Number::Real(b)) => (NumberBinaryOperand::Real(a, b)),
        (Number::Rational(a1, a2), Number::Rational(b1, b2)) => {
            NumberBinaryOperand::Rational(a1, a2, b1, b2)
        }
    }
}

impl<R: RealNumberInternalTrait> NumberBinaryOperand<R> {
    pub fn lhs(&self) -> Number<R> {
        match self {
            NumberBinaryOperand::Integer(a, _) => Number::Integer(*a),
            NumberBinaryOperand::Real(a, _) => Number::Real(*a),
            NumberBinaryOperand::Rational(a1, a2, _, _) => Number::Rational(*a1, *a2),
        }
    }

    pub fn rhs(&self) -> Number<R> {
        match self {
            NumberBinaryOperand::Integer(_, b) => Number::Integer(*b),
            NumberBinaryOperand::Real(_, b) => Number::Real(*b),
            NumberBinaryOperand::Rational(_, _, b1, b2) => Number::Rational(*b1, *b2),
        }
    }
}

impl<R: RealNumberInternalTrait> std::ops::Add<Number<R>> for Number<R> {
    type Output = Number<R>;
    fn add(self, rhs: Number<R>) -> Number<R> {
        match upcast_oprands((self, rhs)) {
            NumberBinaryOperand::Integer(a, b) => Number::Integer(a + b),
            NumberBinaryOperand::Real(a, b) => Number::Real(a + b),
            NumberBinaryOperand::Rational(a1, a2, b1, b2) => {
                Number::Rational(a1 * b2 + a2 * b1, a2 * b2)
            }
        }
    }
}

impl<R: RealNumberInternalTrait> std::ops::Sub<Number<R>> for Number<R> {
    type Output = Number<R>;
    fn sub(self, rhs: Number<R>) -> Number<R> {
        match upcast_oprands((self, rhs)) {
            NumberBinaryOperand::Integer(a, b) => Number::Integer(a - b),
            NumberBinaryOperand::Real(a, b) => Number::Real(a - b),
            NumberBinaryOperand::Rational(a1, a2, b1, b2) => {
                Number::Rational(a1 * b2 - a2 * b1, a2 * b2)
            }
        }
    }
}

impl<R: RealNumberInternalTrait> std::ops::Mul<Number<R>> for Number<R> {
    type Output = Number<R>;
    fn mul(self, rhs: Number<R>) -> Number<R> {
        match upcast_oprands((self, rhs)) {
            NumberBinaryOperand::Integer(a, b) => Number::Integer(a * b),
            NumberBinaryOperand::Real(a, b) => Number::Real(a * b),
            NumberBinaryOperand::Rational(a1, a2, b1, b2) => Number::Rational(a1 * b1, a2 * b2),
        }
    }
}

impl<R: RealNumberInternalTrait> std::ops::Div<Number<R>> for Number<R> {
    type Output = Result<Number<R>>;
    fn div(self, rhs: Number<R>) -> Self::Output {
        match upcast_oprands((self, rhs)) {
            NumberBinaryOperand::Integer(a, b) => {
                check_division_by_zero(b)?;
                match a % b {
                    0 => Ok(Number::Integer(a / b)),
                    _ => Ok(Number::Rational(a, b)),
                }
            }
            NumberBinaryOperand::Real(a, b) => Ok(Number::Real(a / b)),
            NumberBinaryOperand::Rational(a1, a2, b1, b2) => {
                check_division_by_zero(b1)?;
                check_division_by_zero(a2)?;
                check_division_by_zero(b2)?;
                Ok(Number::Rational(a1 * b2, a2 * b1))
            }
        }
    }
}

impl<R: RealNumberInternalTrait> Number<R> {
    pub fn sqrt(self) -> Self {
        match self {
            Number::Integer(num) => Number::Real(R::from(num).unwrap().sqrt()),
            Number::Real(num) => Number::Real(num.sqrt()),
            Number::Rational(a, b) => {
                Number::Real(R::from(a).unwrap() / R::from(b).unwrap().sqrt())
            }
        }
    }
    pub fn floor(self) -> Self {
        match self {
            Number::Integer(num) => Number::Integer(num),
            Number::Real(num) => Number::Real(num.floor()),
            Number::Rational(a, b) => Number::Integer({
                let quot = a / b;
                if quot >= 0 || quot * b == a {
                    quot
                } else {
                    quot - 1
                }
            }),
        }
    }

    pub fn ceiling(self) -> Self {
        match self {
            Number::Integer(num) => Number::Integer(num),
            Number::Real(num) => Number::Real(num.ceil()),
            Number::Rational(a, b) => Number::Integer({
                let quot = a / b;
                if quot <= 0 || quot * b == a {
                    quot
                } else {
                    quot + 1
                }
            }),
        }
    }

    pub fn floor_quotient(self, rhs: Self) -> Result<Self> {
        Ok((self / rhs)?.floor())
    }

    pub fn floor_remainder(self, rhs: Self) -> Result<Self> {
        Ok(self - self.floor_quotient(rhs)? * rhs)
    }

    // Return an exact number that is numerically closest to the given number
    pub fn exact(self) -> Result<Self> {
        match self {
            Number::Real(num) => match num.round().to_i32() {
                Some(i) => Ok(Number::Integer(i)),
                None => logic_error!("cannot be converted to an exact number"),
            },
            exact => Ok(exact),
        }
    }
}
#[test]
fn number_floor() {
    assert_eq!(Number::<f32>::Integer(5).floor(), Number::Integer(5));
    assert_eq!(Number::<f32>::Rational(28, 3).floor(), Number::Integer(9));
    assert_eq!(Number::<f32>::Rational(-43, 7).floor(), Number::Integer(-7));
    assert_eq!(Number::<f32>::Rational(-15, 5).floor(), Number::Integer(-3));
    assert_eq!(Number::<f32>::Real(3.8).floor(), Number::Real(3.0));
    assert_eq!(Number::<f32>::Real(-5.3).floor(), Number::Real(-6.0));
}
#[test]
fn number_ceiling() {
    assert_eq!(Number::<f32>::Integer(5).ceiling(), Number::Integer(5));
    assert_eq!(
        Number::<f32>::Rational(28, 3).ceiling(),
        Number::Integer(10)
    );
    assert_eq!(
        Number::<f32>::Rational(-43, 7).ceiling(),
        Number::Integer(-6)
    );
    assert_eq!(
        Number::<f32>::Rational(-15, 5).ceiling(),
        Number::Integer(-3)
    );
    assert_eq!(Number::<f32>::Real(3.8).ceiling(), Number::Real(4.0));
    assert_eq!(Number::<f32>::Real(-5.3).ceiling(), Number::Real(-5.0));
}
#[test]
fn number_floor_quotient() {
    assert_eq!(
        Number::<f32>::Integer(5).floor_quotient(Number::Integer(2)),
        Ok(Number::Integer(2))
    );
    assert_eq!(
        Number::<f32>::Integer(-5).floor_quotient(Number::Integer(2)),
        Ok(Number::Integer(-3))
    );
    assert_eq!(
        Number::<f32>::Integer(5).floor_quotient(Number::Integer(-2)),
        Ok(Number::Integer(-3))
    );
    assert_eq!(
        Number::<f32>::Integer(-5).floor_quotient(Number::Integer(-2)),
        Ok(Number::Integer(2))
    );
    assert_eq!(
        Number::<f32>::Rational(25, 2).floor_quotient(Number::Integer(3)),
        Ok(Number::Integer(4))
    );
    assert_eq!(
        Number::<f32>::Rational(-25, 2).floor_quotient(Number::Integer(3)),
        Ok(Number::Integer(-5))
    );
    assert_eq!(
        Number::<f32>::Rational(33, 7).floor_quotient(Number::Rational(5, 2)),
        Ok(Number::Integer(1))
    );
    assert_eq!(
        Number::<f32>::Real(5.0).floor_quotient(Number::Real(2.0)),
        Ok(Number::Real(2.0))
    );
    assert_eq!(
        Number::<f32>::Integer(-5).floor_quotient(Number::Real(2.0)),
        Ok(Number::Real(-3.0))
    );
    assert_eq!(
        Number::<f32>::Real(5.0).floor_quotient(Number::Integer(-2)),
        Ok(Number::Real(-3.0))
    );
    assert_eq!(
        Number::<f32>::Rational(-15, 2).floor_quotient(Number::Real(-3.0)),
        Ok(Number::Real(2.0))
    );
}
#[test]
fn number_floor_remainder() {
    assert_eq!(
        Number::<f32>::Integer(5).floor_remainder(Number::Integer(2)),
        Ok(Number::Integer(1))
    );
    assert_eq!(
        Number::<f32>::Integer(-5).floor_remainder(Number::Integer(2)),
        Ok(Number::Integer(1))
    );
    assert_eq!(
        Number::<f32>::Integer(5).floor_remainder(Number::Integer(-2)),
        Ok(Number::Integer(-1))
    );
    assert_eq!(
        Number::<f32>::Integer(-5).floor_remainder(Number::Integer(-2)),
        Ok(Number::Integer(-1))
    );
    assert_eq!(
        Number::<f32>::Rational(25, 2).floor_remainder(Number::Integer(3)),
        Ok(Number::Rational(1, 2))
    );
    assert_eq!(
        Number::<f32>::Rational(-25, 2).floor_remainder(Number::Integer(3)),
        Ok(Number::Rational(5, 2))
    );
    assert_eq!(
        Number::<f32>::Rational(33, 7).floor_remainder(Number::Rational(5, 2)),
        Ok(Number::Rational(31, 14))
    );
    assert_eq!(
        Number::<f32>::Real(5.0).floor_remainder(Number::Real(2.0)),
        Ok(Number::Real(1.0))
    );
    assert_eq!(
        Number::<f32>::Integer(-5).floor_remainder(Number::Real(2.0)),
        Ok(Number::Real(1.0))
    );
    assert_eq!(
        Number::<f32>::Real(5.0).floor_remainder(Number::Integer(-2)),
        Ok(Number::Real(-1.0))
    );
    assert_eq!(
        Number::<f32>::Rational(-15, 2).floor_remainder(Number::Real(-3.0)),
        Ok(Number::Real(-1.5))
    );
}

#[test]
fn number_exact() {
    assert_eq!(Number::<f32>::Integer(5).exact(), Ok(Number::Integer(5)));
    assert_eq!(Number::<f32>::Real(5.3).exact(), Ok(Number::Integer(5)));
    assert_eq!(Number::<f32>::Real(5.8).exact(), Ok(Number::Integer(6)));
    assert_eq!(Number::<f32>::Real(-5.8).exact(), Ok(Number::Integer(-6)));
    assert_eq!(Number::<f32>::Real(-5.3).exact(), Ok(Number::Integer(-5)));
    assert_eq!(
        Number::<f32>::Real(1e30).exact(),
        Err(SchemeError {
            location: None,
            category: ErrorType::Logic,
            message: "cannot be converted to an exact number".to_string()
        })
    );
    assert_eq!(
        Number::<f32>::Rational(7, 3).exact(),
        Ok(Number::Rational(7, 3))
    );
}

pub type ArgVec<R, E> = SmallVec<[Value<R, E>; 4]>;

pub trait DeepCloneWithEnv<R: RealNumberInternalTrait, E: IEnvironment<R>> {
    fn deep_clone_with_env(&self, env: Rc<E>) -> Self;
}

impl<R: RealNumberInternalTrait, E: IEnvironment<R>, T: DeepCloneWithEnv<R, E>>
    DeepCloneWithEnv<R, E> for Vec<T>
{
    fn deep_clone_with_env(&self, env: Rc<E>) -> Self {
        self.iter()
            .map(|t| t.deep_clone_with_env(env.clone()))
            .collect()
    }
}
impl<R: RealNumberInternalTrait, E: IEnvironment<R>> DeepCloneWithEnv<R, E> for Pair<R, E> {
    fn deep_clone_with_env(&self, env: Rc<E>) -> Self {
        Self::new(
            self.car.deep_clone_with_env(env.clone()),
            self.cdr.deep_clone_with_env(env),
        )
    }
}

#[derive(Clone, PartialEq)]
pub enum BuildinProcedurePointer<R: RealNumberInternalTrait, E: IEnvironment<R>> {
    Pure(fn(ArgVec<R, E>) -> Result<Value<R, E>>),
    Impure(fn(ArgVec<R, E>, Rc<E>) -> Result<Value<R, E>>),
}
impl<R: RealNumberInternalTrait, E: IEnvironment<R>> BuildinProcedurePointer<R, E> {
    pub fn apply(&self, args: ArgVec<R, E>, env: &Rc<E>) -> Result<Value<R, E>> {
        match &self {
            Self::Pure(pointer) => pointer(args),
            Self::Impure(pointer) => pointer(args, env.clone()),
        }
    }
}
#[derive(Clone, PartialEq)]
pub struct BuildinProcedure<R: RealNumberInternalTrait, E: IEnvironment<R>> {
    pub name: &'static str,
    pub parameters: ParameterFormals,
    pub pointer: BuildinProcedurePointer<R, E>,
}

impl<R: RealNumberInternalTrait, E: IEnvironment<R>> Display for BuildinProcedure<R, E> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "<build-in procedure ({})>", self.name)
    }
}
impl<R: RealNumberInternalTrait, E: IEnvironment<R>> Debug for BuildinProcedure<R, E> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Procedure<R: RealNumberInternalTrait, E: IEnvironment<R>> {
    User(SchemeProcedure, Rc<E>),
    Buildin(BuildinProcedure<R, E>),
}

impl<R: RealNumberInternalTrait, E: IEnvironment<R>> DeepCloneWithEnv<R, E> for Procedure<R, E> {
    fn deep_clone_with_env(&self, env: Rc<E>) -> Self {
        match self {
            Procedure::User(scheme_procedure, ..) => Procedure::User(scheme_procedure.clone(), env),
            other => other.clone(),
        }
    }
}

impl ParameterFormals {}

impl<R: RealNumberInternalTrait, E: IEnvironment<R>> Procedure<R, E> {
    pub fn new_buildin_pure(
        name: &'static str,
        parameters: ParameterFormals,
        pointer: fn(ArgVec<R, E>) -> Result<Value<R, E>>,
    ) -> Self {
        Self::Buildin(BuildinProcedure {
            name,
            parameters,
            pointer: BuildinProcedurePointer::Pure(pointer),
        })
    }
    pub fn new_buildin_impure(
        name: &'static str,
        parameters: ParameterFormals,
        pointer: fn(ArgVec<R, E>, Rc<E>) -> Result<Value<R, E>>,
    ) -> Self {
        Self::Buildin(BuildinProcedure {
            name,
            parameters,
            pointer: BuildinProcedurePointer::Impure(pointer),
        })
    }
    pub fn get_parameters(&self) -> &ParameterFormals {
        match &self {
            Procedure::User(user, ..) => &user.0,
            Procedure::Buildin(buildin) => &buildin.parameters,
        }
    }
}

impl<R: RealNumberInternalTrait, E: IEnvironment<R>> Display for Procedure<R, E> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self {
            Procedure::User(procedure, ..) => write!(f, "{}", procedure),
            Procedure::Buildin(fp) => write!(f, "{}", fp),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueReference<T> {
    Immutable(Rc<T>),
    Mutable(Rc<RefCell<T>>),
}

impl<T> ValueReference<T> {
    pub fn new_immutable(t: T) -> Self {
        Self::Immutable(Rc::new(t))
    }
    pub fn new_mutable(t: T) -> Self {
        Self::Mutable(Rc::new(RefCell::new(t)))
    }
    pub fn as_ref<'a>(&'a self) -> Box<dyn 'a + Deref<Target = T>> {
        match self {
            ValueReference::Immutable(t) => Box::new(t.as_ref()),
            ValueReference::Mutable(t) => Box::new(t.borrow()),
        }
    }
    pub fn as_mut<'a>(&'a self) -> Result<RefMut<'a, T>> {
        match self {
            ValueReference::Immutable(_) => Err(SchemeError {
                location: None,
                category: ErrorType::Logic,
                message: "expect a mutable reference, get a immutable reference!".to_string(),
            }),
            ValueReference::Mutable(t) => Ok(t.borrow_mut()),
        }
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Immutable(a), Self::Immutable(b)) => Rc::ptr_eq(a, b),
            (Self::Mutable(a), Self::Mutable(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl<R: RealNumberInternalTrait, E: IEnvironment<R>, T: DeepCloneWithEnv<R, E>>
    DeepCloneWithEnv<R, E> for ValueReference<T>
{
    fn deep_clone_with_env(&self, env: Rc<E>) -> Self {
        match self {
            ValueReference::Immutable(value) => {
                ValueReference::Immutable(Rc::new(value.deep_clone_with_env(env)))
            }
            ValueReference::Mutable(value) => ValueReference::Mutable(Rc::new(RefCell::new(
                value.borrow().deep_clone_with_env(env),
            ))),
        }
    }
}

#[macro_export]
macro_rules! match_expect_type {
    ($value:expr, $type:pat => $inner: expr, $type_name:expr) => {
        match $value {
            $type => Ok($inner),
            v => Err(SchemeError {
                location: None,
                category: ErrorType::Logic,
                message: format!("expect a {}, got {}", $type_name, v),
            }),
        }
    };
}
#[test]
fn macro_match_expect_type() {
    assert_eq!(
        match_expect_type!(
            Value::<f32, StandardEnv<_>>::Number(Number::Integer(5)),
            Value::Number(Number::Integer(i)) => i, "integer"
        ),
        Ok(5)
    );
    assert_eq!(
        match_expect_type!(
            Value::<f32, StandardEnv<_>>::Number(Number::Integer(1)),
            Value::Number(Number::Integer(i)) => i + 3, "integer"
        ),
        Ok(4)
    );
    assert_eq!(
        match_expect_type!(
            Value::<f32, StandardEnv<_>>::Number(Number::Integer(5)),
            Value::String(s) => s, "string"
        ),
        Err(SchemeError {
            location: None,
            category: ErrorType::Logic,
            message: "expect a string, got 5".to_string(),
        })
    );
}
#[derive(Debug, Clone, PartialEq)]
pub enum Value<R: RealNumberInternalTrait, E: IEnvironment<R>> {
    Number(Number<R>),
    Boolean(bool),
    Character(char),
    String(String),
    Symbol(String),
    Procedure(Procedure<R, E>),
    Vector(ValueReference<Vec<Value<R, E>>>),
    Pair(Box<Pair<R, E>>),
    EmptyList,
    Void,
}

impl<R: RealNumberInternalTrait, E: IEnvironment<R>> Value<R, E> {
    pub fn expect_number(self) -> Result<Number<R>> {
        match_expect_type!(self, Value::Number(number) => number, "number")
    }
    pub fn expect_integer(self) -> Result<i32> {
        match_expect_type!(self, Value::Number(Number::Integer(i)) => i, "integer")
    }
    pub fn expect_real(self) -> Result<R> {
        match_expect_type!(self, Value::Number(Number::Real(r)) => r, "real")
    }
    pub fn expect_vector(self) -> Result<ValueReference<Vec<Value<R, E>>>> {
        match_expect_type!(self, Value::Vector(vector) => vector, "vector")
    }
    pub fn expect_list_or_pair(self) -> Result<Pair<R, E>> {
        match_expect_type!(self, Value::Pair(list) => *list, "list/pair")
    }
    pub fn expect_string(self) -> Result<String> {
        match_expect_type!(self, Value::String(string) => string, "string")
    }
    pub fn expect_symbol(self) -> Result<String> {
        match_expect_type!(self, Value::Symbol(string) => string, "symbol")
    }
    pub fn expect_procedure(self) -> Result<Procedure<R, E>> {
        match_expect_type!(self, Value::Procedure(procedure) => procedure, "procedure")
    }
    pub fn expect_boolean(self) -> Result<bool> {
        match_expect_type!(self, Value::Boolean(boolean) => boolean, "boolean")
    }
}

impl<R: RealNumberInternalTrait, E: IEnvironment<R>> Display for Value<R, E> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Value::Number(num) => write!(f, "{}", num),
            Value::Symbol(symbol) => write!(f, "{}", symbol),
            Value::Procedure(p) => write!(f, "{}", p),
            Value::Void => write!(f, "Void"),
            Value::Boolean(true) => write!(f, "#t"),
            Value::Boolean(false) => write!(f, "#f"),
            Value::Character(c) => write!(f, "#\\{}", c),
            Value::String(ref s) => write!(f, "\"{}\"", s),
            Value::Vector(vec) => write!(
                f,
                "#({})",
                join(vec.as_ref().iter().map(|v| format!("{}", v)), " ")
            ),
            Value::Pair(list) => write!(f, "{}", list),
            Value::EmptyList => write!(f, "()"),
        }
    }
}

impl<R: RealNumberInternalTrait, E: IEnvironment<R>> DeepCloneWithEnv<R, E> for Value<R, E> {
    fn deep_clone_with_env(&self, env: Rc<E>) -> Self {
        match self {
            Self::Procedure(procedure) => Self::Procedure(procedure.deep_clone_with_env(env)),
            Self::Vector(value_ref) => Self::Vector(value_ref.deep_clone_with_env(env)),
            Self::Pair(pair) => Self::Pair(Box::new(pair.as_ref().deep_clone_with_env(env))),
            other => other.clone(),
        }
    }
}

fn check_division_by_zero(num: i32) -> Result<()> {
    match num {
        0 => logic_error!("division by exact zero"),
        _ => Ok(()),
    }
}
