#![allow(non_snake_case, non_upper_case_globals)]

use js_sys::{self, Array};
use std::collections::HashMap;
use nerdle_solver::{Key, KeyStream, check_equation, EquationError, Equation, Possibility, PossibilityMatrix};
use std::str::Chars;
use std::fmt;
use std::iter::zip;
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

fn parse_js_guesses(equations: Array, colors: Array) -> PossibilityMatrix {
    let mut matrix = PossibilityMatrix::blank(8);

    for (equation_js, colors_js) in zip(equations.iter(), colors.iter()) {
        let eqn = equation_js.as_string().unwrap();
        let mut colors = vec![];
        for c in colors_js.as_string().unwrap().chars() {
            colors.push(
                match c {
                    'c' => Possibility::Certain,
                    'p' => Possibility::Unknown,
                    'a' => Possibility::Impossible,
                    _ => panic!("unknwon char! {}", c),
                }
            )
        }
        let eqn = Equation::parse(eqn.as_str()).unwrap();
        matrix.feed(&eqn, colors.as_slice());
    }

    matrix
}

#[wasm_bindgen]
pub fn bestSolutions(equations: Array, colors: Array, len: usize) -> Array {
    let matrix = parse_js_guesses(equations, colors);
    let mut all_answers = vec![];
    matrix.solutions(|e| {
        all_answers.push(e.clone());
        true
    });

    let mut entropy_counts = vec![];
    let size = all_answers.len() as f64;
    for guess in &all_answers {
        let mut distribution = HashMap::<u32, f64>::new();
        for answer in &all_answers {
            let result = answer.compute_hint_id(guess);
            match distribution.get_mut(&result) {
                Some(val) => *val += 1.,
                None => {
                    distribution.insert(result, 1.);
                },
            }
        }
        let mut sum = 0.;
        for &count in distribution.values() {
            let probability = count / size;
            sum += probability * probability.log2();
        }
        entropy_counts.push((sum, guess));
    }

    entropy_counts.sort_by(|(a, _), (b, _)| a.partial_cmp(b).unwrap());

    let answers = Array::new();
    for (_, answer) in entropy_counts.iter().take(len) {
        let s = format!("{}", answer);
        let j = JsValue::from_str(s.as_str());
        answers.push(&j);
    }

    answers
}

#[wasm_bindgen]
pub fn possibleSolutions(equations: Array, colors: Array, offset: i32, mut len: i32) -> Array {
    
    let matrix = parse_js_guesses(equations, colors);

    let answers = Array::new();
    let mut i = 0;
    matrix.solutions(|e| {
        if i >= offset && len > 0 {
            let s = format!("{}", e);
            let j = JsValue::from_str(s.as_str());
            answers.push(&j);
            len -= 1;
        }
        i += 1;
        true
    });

    answers
}

#[wasm_bindgen]
pub fn countSolutions(equations: Array, colors: Array) -> u32 {
    let mut count = 0;
    let matrix = parse_js_guesses(equations, colors);
    matrix.solutions(|_| {
        count += 1;
        true
    });
    count
}
