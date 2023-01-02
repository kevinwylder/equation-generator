#![allow(non_snake_case, non_upper_case_globals)]

use nerdle_solver::{Key, KeyStream, check_equation, EquationError};
use std::str::Chars;
use std::fmt;
use wasm_bindgen::prelude::*;

struct EquationChecker<'a> {
    equation: Chars<'a>,
    current: Option<Key>,
    pos: usize,
}

impl EquationChecker<'_> {
    fn next(&mut self) {
        match self.equation.next() {
            Some(c) => {
                self.pos += 1;
                match Key::parse(c) {
                    Ok(k) => self.current = Some(k),
                    Err(_) => self.current = None
                }
            },
            None => self.current = None
        }
    }
}

impl <'a> From<&'a str> for EquationChecker<'a> {
    fn from(s: &'a str) -> EquationChecker<'a> {
        let mut c = EquationChecker{
            equation: s.chars(),
            current: None,
            pos: 0,
        };
        c.next();
        c.pos = 0;
        c
    }
}

impl KeyStream for EquationChecker<'_> {
    fn next_key(&mut self) -> Option<Key> {
        let now = self.current;
        self.next();
        now
    }

    fn peek_key(&mut self) -> Option<Key> {
        self.current
    }
}

impl fmt::Display for EquationChecker<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for _ in 0..(self.pos-1).max(0) {
            write!(f, "-")?;
        }
        write!(f, "^")
    }
}

#[wasm_bindgen]
pub fn checkCompiles(equation: &str) -> String {
    let mut stream: EquationChecker = equation.into();
    let result = match check_equation(&mut stream) {
        Some(EquationError::Syntax(e)) => format!("syntax error {:?} in spot {}", e, stream.pos + 1),
        Some(EquationError::Value(e)) => format!("value error {:?}", e),
        None => {
            if equation.len() == 8 {
                "".into()
            } else {
                "incomplete".into()
            }
        }
    };
    result
}
