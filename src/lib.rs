#![allow(dead_code)]
use std::fmt::Write;
use std::iter;
use std::ptr;
use std::fmt;
use std::ops;

// 1. equation template iterator (operator_next)
//
// generate a sequence of equations using the chars
//                       1+-*/=
// this yields 6^8=1,679,616 strings (leftmost and rightmost must be "1")
//
// 2. operator possibility matrix check
//
// Use the constraint matrix to rule out the operator placements.
// For integers, verify if *any* int can go in the 1 slots
//
// 3. compile (or back to 1)
//
// rules out equations that have sequential operators, stuff like missing = or duplicate =. 
// More expensive than the operator matrix check so do this one second.
//
// 4. bounds_check (or back to 1)
//
// Traverse the AST and cross reference the possibility matrix to determine if any set of integers
// theoretically could satisfy the equation
//
// 5. integer iterator (int_next)
//
// Iterate over the integers - eliminating by the constraint matrix in integer locations now
//
// 6. compile again (or back to 5)
//
// rules out leading 0 and division by zero errors
//
// 7. evaluate (or back to 5)
//
// Actually verify if the LHS = RHS

#[derive(Copy,Clone,Debug,PartialEq)]
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


    // integer_next increments the equation value without modifying the AST
    // returns true if the key was either rolled over, or is an operator
    fn integer_next(&mut self) -> bool {
        match *self {
            Key::Digit(d) => {
                assert!(d < 10, "key index out of bounds");
                if d != 9 {
                    *self = Key::Digit(d+1);
                    false
                } else {
                    *self = Key::Digit(0);
                    true
                }
            },
            _ => true,
        }
    }

}

#[derive(Copy,Clone,PartialEq,Debug)]
enum Possibility {
    Certain,
    Impossible,
    Unknown,
}

struct PossibilityMatrix {
    // 15 keys * 10 slots
    matrix: [Possibility; 150],
}

impl ops::Index<(usize, Key)> for PossibilityMatrix {
    type Output = Possibility;

    fn index(&self, idx: (usize, Key)) -> &Possibility {
        let (slot, key) = idx;
        assert!(slot < 15, "slot index out of bounds");
        &self.matrix[Self::key_idx(key) + 15 * slot]
    }
}

impl ops::IndexMut<(usize, Key)> for PossibilityMatrix {
    fn index_mut(&mut self, idx: (usize, Key)) -> &mut Possibility {
        let (slot, key) = idx;
        assert!(slot < 15, "slot index out of bounds");
        &mut self.matrix[Self::key_idx(key) + 15 * slot]
    }
}

impl PossibilityMatrix {

    const OPERATOR_KEYS: [Key; 5] = [Key::Plus, Key::Minus, Key::Multiply, Key::Divide, Key::Equal];
    const DIGIT_KEYS: [Key; 10] = [
        Key::Digit(0), Key::Digit(1), Key::Digit(2), Key::Digit(3), Key::Digit(4),
        Key::Digit(5), Key::Digit(6), Key::Digit(7), Key::Digit(8), Key::Digit(9),
    ];
    const ALL_KEYS: [Key; 15] = [
        Key::Plus, Key::Minus, Key::Multiply, Key::Divide, Key::Equal,
        Key::Digit(0), Key::Digit(1), Key::Digit(2), Key::Digit(3), Key::Digit(4),
        Key::Digit(5), Key::Digit(6), Key::Digit(7), Key::Digit(8), Key::Digit(9),
    ];

    // key_idx assigns a numeric value to the key between 0 and 14 (inclusive) for array indexing
    fn key_idx(k: Key) -> usize {
        match k {
            Key::Digit(d) => {
                assert!(d < 10, "key index out of bounds");
                d as usize
            },
            Key::Plus => 10,
            Key::Minus => 11,
            Key::Multiply => 12,
            Key::Divide => 13,
            Key::Equal => 14,
        }
    }

    pub fn blank() -> Self {
        let mut matrix = Self{
            matrix: [Possibility::Unknown; 150],
        };

        // assign impossible to operators on the edges
        for key in Self::OPERATOR_KEYS {
            matrix.eliminate(0, key);
            matrix.eliminate(9, key);
        }
        // also leading zero is impossible
        matrix.eliminate(0, Key::Digit(0));
        
        matrix
    }
    
    pub fn set_certain(&mut self, slot: ProgIndex, key: Key) {
        // sanity check
        assert_ne!(self[(slot, key)], Possibility::Impossible, "{:?} in slot {} was previously impossible", key, slot);

        // set the whole slot
        for eliminate_key in Self::ALL_KEYS {
            if key == eliminate_key {
                self[(slot, eliminate_key)] = Possibility::Certain
            } else {
                // If you are certain of a value, all other keys are impossible in that slot
                self.eliminate(slot, eliminate_key)
            }
        }

        if let Key::Digit(0) = key {
            // can't have an operator to the left of the 0
            for operator in Self::OPERATOR_KEYS {
                self.eliminate(slot - 1, operator)
            }
            return
        }

        if let Key::Equal = key {
            // can't have multiple equals signs
            self.eliminate_everywhere(key)
        }

        // if the key was an operator, the surrounding slots aren't possible operators either
        if Self::OPERATOR_KEYS.contains(&key) {
            for operator in Self::OPERATOR_KEYS {
                self.eliminate(slot - 1, operator);
                self.eliminate(slot + 1, operator);
            }
            // also the next digit can't be a leading 0
            self.eliminate(slot + 1, Key::Digit(0));
            return
        }
    }

    pub fn eliminate(&mut self, slot: ProgIndex, key: Key) {
        // sanity check
        assert_ne!(self[(slot, key)], Possibility::Certain, "{:?} in slot {} was previously certain", key, slot);

        self[(slot, key)] = Possibility::Impossible
    }

    pub fn eliminate_everywhere(&mut self, key: Key) {
        for slot in 0..10 {
            // do not overwrite certain cells
            let cell = &mut self[(slot, key)];
            if *cell != Possibility::Certain {
                *cell = Possibility::Impossible
            }
        }
    }
    
    fn first_possible_digit<I: iter::Iterator<Item = i32>>(&self, slot: usize, range: I) -> Option<i32> {
        for i in range {
            match self.matrix[slot * 15 + i as usize] {
                Possibility::Impossible => continue,
                _ => return Some(i)
            }
        }
        None
    }

    fn int_bounds(&self, start: ProgIndex, width: IntWidth) -> Option<(i32, i32)> {
        let mut min = 0;
        let mut max = 0;
        for slot in start..(start+width) {
            let (min_range, max_range) = if slot == 0 {
                (1..10, (1..10).rev())
            } else {
                (0..10, (0..10).rev())
            };
            match self.first_possible_digit(slot, min_range) {
                Some(min_digit) => min = min * 10 + min_digit,
                None => return None,
            };
            match self.first_possible_digit(slot, max_range) {
                Some(max_digit) => max = max * 10 + max_digit,
                None => return None,
            };
        }
        Some((min, max))
    }

    fn any_digit(&self, slot: ProgIndex) -> Possibility {
        for key in Self::DIGIT_KEYS {
            let poss = self[(slot, key)];
            if poss != Possibility::Impossible {
                return poss
            }
        }
        Possibility::Impossible
    }

    // check if the operators in this equation can satisfy the constraint matrix
    fn operator_check(&mut self, e: &Equation) -> bool {
        for (slot, key) in e.keys.iter().enumerate() {
            match *key {
                Key::Digit(_) => {
                    for operator in Self::OPERATOR_KEYS {
                        if self[(slot, operator)] == Possibility::Certain {
                            return false
                        }
                    }
                },
                _ => {
                    if self[(slot, *key)] == Possibility::Impossible {
                        return false
                    }
                }
            }
        }
        return true
    }
}

struct Equation {
    keys: [Key; 10],
    idx: usize,
}

impl Equation {
    
    fn new() -> Self {
        Self {
            keys: [Key::Digit(1); 10],
            idx: 0,
        }
    }

    fn parse(s: &str) -> Result<Self, char> {
        let mut e = Self::new();
        for (i, c) in s.chars().enumerate() {
            e.keys[i] = Key::parse(c)?;
        }
        Ok(e)
    }

    fn next_equation_template(&mut self, constraints: &PossibilityMatrix) -> bool {
        println!("hi");
        for slot in (1..9).rev() {
            println!("slot {}", slot);
            let mut has_rolled_over = false;
            let key = &mut self.keys[slot];
            let current = match key {
                Key::Digit(_) => constraints.any_digit(slot),
                _ => constraints[(slot, *key)]
            };
            if current == Possibility::Certain {
                continue
            }
            loop {
                let possibility = match key {
                    Key::Digit(_) => {
                        *key = Key::Equal;
                        constraints[(slot, *key)]
                    },
                    Key::Equal => {
                        *key = Key::Plus;
                        constraints[(slot, *key)]
                    }
                    Key::Plus => {
                        *key = Key::Minus;
                        constraints[(slot, *key)]
                    },
                    Key::Minus => {
                        *key = Key::Multiply;
                        constraints[(slot, *key)]
                    },
                    Key::Multiply => {
                        *key = Key::Divide;
                        constraints[(slot, *key)]
                    },
                    Key::Divide => {
                        *key = Key::Digit(1);
                        if has_rolled_over {
                            panic!("I rolled over trying to find a legal equation template! {}", slot)
                        }
                        has_rolled_over = true;
                        // digits are weird, they're only possible if any digit is possible
                        constraints.any_digit(slot)
                    },
                };
                if match possibility {
                    Possibility::Certain => return true,
                    Possibility::Impossible => false,
                    Possibility::Unknown => true,
                } {
                    if has_rolled_over {
                        break
                    }
                    return true
                }
            }
        }
        false
    }

    fn check_compile(&mut self) -> Result<AST, CompileError> {
        self.idx = 0;
        for ref mut key in self.keys {
            match key {
                Key::Digit(d) => *d = 1,
                _ => (),
            }
        }
        let ast = AST::compile(self)?;
        match &ast {
            AST::Equals(_, _) => Ok(ast),
            _ => Err(CompileError::MissingEquals),
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
    MissingEquals,
}

#[derive(PartialEq,Debug)]
enum EvalError {
    NotEquals,
    Fraction,
    DivZero,
}


enum AST {
    Equals(Box<AST>, Box<AST>),
    Upper(Box<AST>, UpperOp, Box<AST>),
    Lower(Box<AST>, LowerOp, Box<AST>),
    Integer(i32, ProgIndex, IntWidth),
}

// ProgIndex is the index into the program where this Key sequence starts
type ProgIndex = usize;
// IntWidth is the width of the integer in Key
type IntWidth = usize;

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

    fn parse_int(&mut self) -> Result<(i32, IntWidth), CompileError> {
        let mut value = match self.next_key() {
            Some(Key::Digit(v)) => v as i32,
            Some(k) => return Err(CompileError::UnexpectedOperator(k)),
            None => return Err(CompileError::EndOfProgram)
        };
        let mut width = 1;
        if value == 0 {
            return Err(CompileError::LeadingZero)
        }
        loop {
            match self.peek_key() {
                Some(Key::Digit(v)) => {
                    self.next_key();
                    width += 1;
                    value = (value * 10) + (v as i32)
                }
                _ => return Ok((value, width))
            }
        }
    }
}

impl KeyStream for Equation {
    fn next_key(&mut self) -> Option<Key> {
        self.idx += 1;
        self.peek_key()
    }
    fn peek_key(&mut self) -> Option<Key> {
        match self.keys.get(self.idx) {
            Some(k) => Some(*k),
            None => None,
        }
    }
}



impl AST {

    pub fn compile<K: KeyStream>(data: &mut K) -> Result<AST, CompileError> {
        let (val, width) = data.parse_int()?;
        let mut tree = AST::Integer(val, 0, width);
        let mut index = width;
        loop {
            match data.next_key() {
                None => return Ok(tree),
                Some(k) => {
                    let (val, width) = data.parse_int()?;
                    let integer = AST::Integer(val, index + 1, width);
                    tree.append(k, integer)?;
                    index += 1 + width;
                }
            }
        }
    }

    // bounds computes the upper and lower bound for each expression
    fn bounds(self, constraints: &PossibilityMatrix) -> Option<(i32, i32)> {
        match self {
            AST::Equals(lhs, rhs) => {
                match (lhs.bounds(constraints), rhs.bounds(constraints)) {
                    (Some((lhs_low, lhs_high)), Some((rhs_low, rhs_high))) => 
                        Some((lhs_low - rhs_high, lhs_high - rhs_low)),
                    _ => None
                }
            }
            AST::Upper(lhs, op, rhs) => {
                match (lhs.bounds(constraints), rhs.bounds(constraints)) {
                    (Some((lhs_low, lhs_high)), Some((rhs_low, rhs_high))) => match op {
                        UpperOp::Plus => Some((lhs_low + rhs_low, lhs_high + rhs_high)),
                        UpperOp::Minus => Some((lhs_low - rhs_high, lhs_high - rhs_low)),
                    },
                    _ => None
                }
            },
            AST::Lower(lhs, op, rhs) => {
                match (lhs.bounds(constraints), rhs.bounds(constraints)) {
                    (Some((lhs_low, lhs_high)), Some((rhs_low, rhs_high))) => match op {
                        LowerOp::Mul => Some((lhs_low * rhs_low, lhs_high * rhs_high)),
                        LowerOp::Div => Some((lhs_low / rhs_high, lhs_high / rhs_low)),
                    }
                    _ => None
                }
            }
            AST::Integer(_, idx, width) => constraints.int_bounds(idx, width)
        }
    }

    
    // https://stackoverflow.com/a/60382120
    // https://github.com/alecmocatta/replace_with/blob/master/src/lib.rs
    fn unsafe_replace_with<F: FnOnce(Self) -> Self>(&mut self, construct: F) {
        unsafe {
            let old_self = ptr::read(self);
            // NOTE: some terrible things may happen if you panic here
            let new_self = construct(old_self);
            ptr::write(self, new_self);
        }
    }

    fn append(&mut self, k: Key, integer: AST) -> Result<(), CompileError> {
        match k {
            Key::Digit(_) => return Err(CompileError::UnexpectedNumber(k)),
            Key::Multiply => self.append_lower(LowerOp::Mul, integer),
            Key::Divide => self.append_lower(LowerOp::Div, integer),
            Key::Plus =>  self.append_upper(UpperOp::Plus, integer),
            Key::Minus => self.append_upper(UpperOp::Minus, integer),
            Key::Equal => self.append_equals(integer)?,
        }
        Ok(())
    }

    fn append_equals(&mut self, integer: AST) -> Result<(), CompileError> {
        match self {
            AST::Equals(_, _) => Err(CompileError::DuplicateEquals),
            _ => {
                self.unsafe_replace_with(
                    |_self| AST::Equals(
                        Box::new(_self),
                        Box::new(integer),
                    )
                );
                Ok(())
            }
        }
    }

    fn append_upper(&mut self, op: UpperOp, integer: AST) {
        let side: &mut Self = match self {
            AST::Equals(_, rhs) => rhs,
            _ => self
        };
        side.unsafe_replace_with(
            |_self| AST::Upper(
                Box::new(_self),
                op,
                Box::new(integer),
            )
        );
    }

    fn append_lower(&mut self, op: LowerOp, integer: AST) {
        let side: &mut Self = match self {
            AST::Equals(_, rhs) => return rhs.append_lower(op, integer),
            AST::Upper(_, _, rhs) => rhs,
            _ => self,
        };
        side.unsafe_replace_with(
            |_self| AST::Lower(
                Box::new(_self),
                op,
                Box::new(integer)
            )
        )
    }

    fn value(&self) -> Result<i32, EvalError> {
        Ok(
            match self {
                AST::Integer(i, _, _) => *i,
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
            AST::Integer(i, _, _) => i.fmt(f)
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
            AST::Integer(i, _, _) => i.fmt(f)
        }
    }
}

impl fmt::Display for Key {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        f.write_char(
            match self {
                Key::Plus => '+',
                Key::Minus => '-',
                Key::Multiply => '*',
                Key::Divide => '/',
                Key::Equal => '=',
                &Key::Digit(d) => (d + '0' as u8) as char
            }
        )
    }
}

impl fmt::Display for Equation {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}{}{}{}{}{}{}{}{}{}", 
            self.keys[0], self.keys[1], self.keys[2], self.keys[3], self.keys[4],
            self.keys[5], self.keys[6], self.keys[7], self.keys[8], self.keys[9],
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str;
    use std::iter;

    impl KeyStream for iter::Peekable<str::Chars<'static>> {

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

    #[test]
    fn test_constraint_matrix() {
        let mut matrix = PossibilityMatrix::blank();
        assert_eq!(matrix[(0, Key::Equal)], Possibility::Impossible);
        assert_eq!(matrix[(0, Key::Digit(0))], Possibility::Impossible);
        assert_eq!(matrix[(9, Key::Equal)], Possibility::Impossible);
        assert_eq!(matrix[(9, Key::Digit(0))], Possibility::Unknown);

        matrix.set_certain(1, Key::Multiply);
        assert_eq!(matrix[(2, Key::Divide)], Possibility::Impossible);
        assert_eq!(matrix[(2, Key::Digit(1))], Possibility::Unknown);
        assert_eq!(matrix[(2, Key::Digit(0))], Possibility::Impossible);

        matrix.eliminate(2, Key::Digit(1));
        assert_eq!(matrix[(2, Key::Digit(1))], Possibility::Impossible);
        assert_eq!(matrix[(2, Key::Digit(2))], Possibility::Unknown);

        // eliminate_everywhere doesn't affect certain cells
        matrix.set_certain(9, Key::Digit(3));
        matrix.eliminate_everywhere(Key::Digit(3));
        for slot in 0..9 {
            assert_eq!(matrix[(slot, Key::Digit(3))], Possibility::Impossible);
        }
        assert_eq!(matrix[(9, Key::Digit(3))], Possibility::Certain);

        matrix.set_certain(4, Key::Digit(0));
        assert_eq!(matrix[(3, Key::Plus)], Possibility::Impossible);
        assert_eq!(matrix[(4, Key::Digit(0))], Possibility::Certain);
        assert_eq!(matrix[(4, Key::Digit(1))], Possibility::Impossible);
    }

    #[test]
    fn test_next_equation_template() {
        let mut matrix = PossibilityMatrix::blank();
        let mut equation = Equation::new();
        assert_eq!(format!("{}", equation), "1111111111");

        equation.next_equation_template(&matrix);
        assert_eq!(format!("{}", equation), "11111111=1");

        matrix.set_certain(8, Key::Equal);
        equation.next_equation_template(&matrix);
        assert_eq!(format!("{}", equation), "111111+1=1");
        equation.next_equation_template(&matrix);
        assert_eq!(format!("{}", equation), "111111-1=1");
        equation.next_equation_template(&matrix);
        assert_eq!(format!("{}", equation), "111111*1=1");
        equation.next_equation_template(&matrix);
        assert_eq!(format!("{}", equation), "111111/1=1");
        equation.next_equation_template(&matrix);
        assert_eq!(format!("{}", equation), "11111+11=1");

        matrix.set_certain(6, Key::Digit(3));
        equation.next_equation_template(&matrix);
        assert_eq!(format!("{}", equation), "11111-11=1");
    }
}

