use ruschm::{
    environment::StandardEnv,
    error::SchemeError,
    interpreter::Interpreter,
    values::Number,
    values::{Pair, Value},
};

#[test]
fn list() -> Result<(), SchemeError> {
    let mut interpreter = Interpreter::<f32, StandardEnv<f32>>::new_with_stdlib();
    assert_eq!(
        interpreter.eval("(list 1 2 3)".chars())?,
        Some(Value::Pair(Box::new(Pair::Some(
            Value::Number(Number::Integer(1)),
            Value::Pair(Box::new(Pair::Some(
                Value::Number(Number::Integer(2)),
                Value::Pair(Box::new(Pair::Some(
                    Value::Number(Number::Integer(3)),
                    Value::Pair(Box::new(Pair::Empty))
                )))
            )))
        ))))
    );

    assert_eq!(
        interpreter.eval("'(1 2 . 3)".chars())?,
        Some(Value::Pair(Box::new(Pair::Some(
            Value::Number(Number::Integer(1)),
            Value::Pair(Box::new(Pair::Some(
                Value::Number(Number::Integer(2)),
                Value::Number(Number::Integer(3))
            )))
        ))))
    );
    Ok(())
}
