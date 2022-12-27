#![allow(dead_code)]
use std::ptr;
use std::fmt;

#[derive(Copy,Clone,Debug)]
enum Key {
    Digit(u8),
    Plus,
    Minus,
    Multiply,
    Divide,
    Equal
}

impl Key {

    fn parse(c: char) -> Result<Key, char> {
        match c {
            '+' => Ok(Key::Plus),
            '-' => Ok(Key::Minus),
            '*' => Ok(Key::Multiply),
            '/' => Ok(Key::Divide),
            '=' => Ok(Key::Equal),
            '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => 
                Ok(Key::Digit((
                    (c as i32) - ('0' as i32)) as u8
                )),
            _ => Err(c)
        }
    }
    
}

#[derive(Debug)]
enum CompileError {
    UnexpectedOperator(Key),
    UnexpectedNumber(Key),
    LeadingZero,
    EndOfProgram,
    DuplicateEquals,
}

#[derive(PartialEq,Debug)]
enum EvalError {
    NotEquals,
    Fraction,
    DivZero,
}

enum AST {
    Integer(i32),
    Lower(Box<AST>, LowerOp, Box<AST>),
    Upper(Box<AST>, UpperOp, Box<AST>),
    Equals(Box<AST>, Box<AST>)
}

enum LowerOp {
    Mul,
    Div
}

enum UpperOp {
    Plus,
    Minus
}

trait KeyStream {
    fn next_key(&mut self) -> Option<Key>;
    fn peek_key(&mut self) -> Option<Key>;
}

impl AST {

    pub fn compile<K: KeyStream>(data: &mut K) -> Result<AST, CompileError> {
        let val = Self::parse_int(data)?;
        let mut tree = AST::Integer(val);
        loop {
            match data.next_key() {
                None => return Ok(tree),
                Some(k) => {
                    let rhs = Self::parse_int(data)?;
                    AST::append(&mut tree, k, rhs)?;
                }
            }
        }
    }

    fn parse_int<K: KeyStream>(data: &mut K) -> Result<i32, CompileError> {
        let mut value = match data.next_key() {
            Some(Key::Digit(v)) => v as i32,
            Some(k) => return Err(CompileError::UnexpectedOperator(k)),
            None => return Err(CompileError::EndOfProgram)
        };
        if value == 0 {
            return Err(CompileError::LeadingZero)
        }
        loop {
            match data.peek_key() {
                Some(Key::Digit(v)) => {
                    data.next_key();
                    value = (value * 10) + (v as i32)
                }
                _ => return Ok(value)
            }
        }
    }
    
    // https://stackoverflow.com/questions/29570781/temporarily-move-out-of-borrowed-content
    // https://github.com/alecmocatta/replace_with/blob/master/src/lib.rs
    fn unsafe_replace_with<F: FnOnce(Self) -> Self>(&mut self, construct: F) {
        unsafe {
            let old_self = ptr::read(self);
            // NOTE: some terrible things may happen if you panic here
            let new_self = construct(old_self);
            ptr::write(self, new_self);
        }
    }

    fn append(&mut self, k: Key, v: i32) -> Result<(), CompileError> {
        match k {
            Key::Digit(_) => return Err(CompileError::UnexpectedNumber(k)),
            Key::Multiply => self.append_lower(LowerOp::Mul, v),
            Key::Divide => self.append_lower(LowerOp::Div, v),
            Key::Plus =>  self.append_upper(UpperOp::Plus, v),
            Key::Minus => self.append_upper(UpperOp::Minus, v),
            Key::Equal => self.append_equals(v)?,
        }
        Ok(())
    }

    fn append_equals(&mut self, v: i32) -> Result<(), CompileError> {
        match self {
            AST::Equals(_, _) => Err(CompileError::DuplicateEquals),
            _ => {
                self.unsafe_replace_with(
                    |_self| AST::Equals(
                        Box::new(_self),
                        Box::new(AST::Integer(v)),
                    )
                );
                Ok(())
            }
        }
    }

    fn append_upper(&mut self, op: UpperOp, v: i32) {
        let side: &mut Self = match self {
            AST::Equals(_, rhs) => rhs,
            _ => self
        };
        side.unsafe_replace_with(
            |_self| AST::Upper(
                Box::new(_self),
                op,
                Box::new(AST::Integer(v))
            )
        );
    }

    fn append_lower(&mut self, op: LowerOp, v: i32) {
        let side: &mut Self = match self {
            AST::Equals(_, rhs) => return rhs.append_lower(op, v),
            AST::Upper(_, _, rhs) => rhs,
            _ => self,
        };
        side.unsafe_replace_with(
            |_self| AST::Lower(
                Box::new(_self),
                op,
                Box::new(AST::Integer(v))
            )
        )
    }

    fn value(&self) -> Result<i32, EvalError> {
        Ok(
            match self {
                AST::Integer(i) => *i,
                AST::Lower(lhs, op, rhs) =>
                    match op {
                        LowerOp::Mul => lhs.value()? * rhs.value()?,
                        LowerOp::Div => {
                            let rhs_value = rhs.value()?;
                            if rhs_value == 0 {
                                return Err(EvalError::DivZero)
                            }
                            let lhs_value = lhs.value()?;
                            if lhs_value % rhs_value != 0 {
                                return Err(EvalError::Fraction)
                            }
                            lhs_value / rhs_value
                        }
                    }
                AST::Upper(lhs, op, rhs) => {
                    match op {
                        UpperOp::Plus => lhs.value()? + rhs.value()?,
                        UpperOp::Minus => lhs.value()? - rhs.value()?
                    }
                }
                AST::Equals(lhs, rhs) => {
                    if lhs.value()? != rhs.value()? {
                        return Err(EvalError::NotEquals)
                    }
                    0
                }
            }
        )
    }

}

impl fmt::Debug for AST {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            AST::Equals(lhs, rhs) => write!(f, "{:?}={:?}", lhs, rhs),
            AST::Upper(lhs, op, rhs) => {
                match op {
                    UpperOp::Plus => write!(f, "({:?}+{:?})", lhs, rhs),
                    UpperOp::Minus => write!(f, "({:?}-{:?})", lhs, rhs),
                }
            },
            AST::Lower(lhs, op, rhs) => {
                match op {
                    LowerOp::Mul => write!(f, "({:?}*{:?})", lhs, rhs),
                    LowerOp::Div => write!(f, "({:?}/{:?})", lhs, rhs)
                }
            },
            AST::Integer(i) => i.fmt(f)
        }
    }
}

impl fmt::Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            AST::Equals(lhs, rhs) => write!(f, "{}={}", lhs, rhs),
            AST::Upper(lhs, op, rhs) => {
                match op {
                    UpperOp::Plus => write!(f, "{}+{}", lhs, rhs),
                    UpperOp::Minus => write!(f, "{}-{}", lhs, rhs),
                }
            },
            AST::Lower(lhs, op, rhs) => {
                match op {
                    LowerOp::Mul => write!(f, "{}*{}", lhs, rhs),
                    LowerOp::Div => write!(f, "{}/{}", lhs, rhs)
                }
            },
            AST::Integer(i) => i.fmt(f)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str;
    use std::iter::{Peekable, Iterator};

    impl KeyStream for Peekable<str::Chars<'static>> {

        fn next_key(&mut self) -> Option<Key> {
            if let Some(c) = self.next() {
                match Key::parse(c) {
                    Ok(k) => Some(k),
                    Err(_) => panic!("cannot parse {}", c),
                }
            } else {
                None
            }
        }

        fn peek_key(&mut self) -> Option<Key> {
            if let Some(c) = self.peek() {
                match Key::parse(*c) {
                    Ok(k) => Some(k),
                    Err(_) => panic!("cannot parse {}", c),
                }
            } else {
                None
            }
        }
    }

    #[test]
    fn test_compile_programs() {
        let tests = vec![
            ("1", "1", Ok(1)),
            ("10", "10", Ok(10)),
            ("2+2", "(2+2)", Ok(4)),
            ("13+2", "(13+2)", Ok(15)),
            ("2*3+4", "((2*3)+4)", Ok(10)),
            ("2+3*4", "(2+(3*4))", Ok(14)),
            ("2+3*4=84", "(2+(3*4))=84", Err(EvalError::NotEquals)),
            ("2+3*4=84-70", "(2+(3*4))=(84-70)", Ok(0)),
        ];

        for (i, test) in tests.iter().enumerate() {
            let (prog, repr, eval) = test;
            let mut stream = prog.chars().peekable();
            let ast = match AST::compile(&mut stream) {
                Err(e) => panic!("program {} `{}` failed to compile - {:?}", i, prog, e),
                Ok(ast) => ast,
            };
            
            assert_eq!(prog.to_string(), format!("{}", ast));
            assert_eq!(repr.to_string(), format!("{:?}", ast));
            assert_eq!(ast.value(), *eval);
        }
    }

    #[test]
    fn test_invalid_programs() {
        let progs = vec![
            "0000",
            "+1",
            "1+",
            "1=+",
            "-1",
            "01",
            "=",
        ];
        for prog in progs {
            let mut stream = prog.chars().peekable();
            if let Ok(_) = AST::compile(&mut stream) {
                panic!("{} should not compile", prog)
            }
        }
    }

}

