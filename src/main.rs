use std::{collections::HashMap, sync::mpsc::channel, thread};

use nerdle_solver::{PossibilityMatrix, Key, Equation};

fn main() {
    let game = if false {
        let mut game = PossibilityMatrix::blank(8);

        // 26-12=14
        game.set_certain(4, Key::Digit(2));
        game.set_certain(5, Key::Equal);
        game.set_certain(6, Key::Digit(1));
        game.eliminate_everywhere(Key::Digit(1));
        game.eliminate_everywhere(Key::Digit(2));
        game.eliminate_everywhere(Key::Digit(6));
        game.eliminate_everywhere(Key::Minus);
        game.eliminate_everywhere(Key::Digit(4));

        // 43-5*7=8
        game.eliminate(1, Key::Digit(3));
        game.eliminate(3, Key::Digit(5));
        game.eliminate(4, Key::Multiply);
        game.eliminate_everywhere(Key::Digit(7));
        game.eliminate_everywhere(Key::Digit(8));

        game
    } else {
        PossibilityMatrix::blank(8)
    };

    let mut answers = vec![];

    game.solutions(|possible_answer| {
        answers.push(possible_answer.clone());
        true
    });
    
    compute_entropy_parallel(20, &answers);
}

fn compute_entropy_parallel(workers: usize, answers: &Vec<Equation>) {

    let mut all_guesses = vec![];
    let guesses = PossibilityMatrix::blank(8);
    guesses.solutions(|s| {
        all_guesses.push(s.clone());
        true
    });

    let (entropy_stream_tx, entropy_stream_rx) = channel();
    let mut threads = vec![];
    for i in 0..workers {
        let tx = entropy_stream_tx.clone();
        let answers = answers.clone();
        let mut partition = vec![];
        for (j, guess) in all_guesses.iter().enumerate() {
            if j % workers == i {
                partition.push(guess.clone());
            }
        }
        threads.push(thread::spawn(move || {
            for guess in partition {
                let mut distribution = HashMap::<u32, f64>::new();
                let mut is_answer = false;
                for answer in &answers {
                    if guess.eq(answer) {
                        is_answer = true
                    }
                    let result = answer.compute_hint_id(&guess);
                    match distribution.get_mut(&result) {
                        Some(val) => *val += 1.,
                        None => {
                            distribution.insert(result, 1.);
                        },
                    }
                };
                let size = answers.len() as f64;
                let mut sum = 0.;
                for &count in distribution.values() {
                    let probability = count / size;
                    sum += probability * probability.log2();
                }
                tx.send((guess, -sum, is_answer)).unwrap();
            }
        }))
    }

    let mut countdown = all_guesses.len();
    let mut answers = vec![];
    let mut best_bits = 0.;
    for (answer, bits, is_answer) in entropy_stream_rx {
        if bits > best_bits {
            println!("{} bits for {}\r", bits, answer);
            best_bits = bits;
        }
        answers.push((bits, is_answer, answer));
        countdown -= 1;
        if countdown == 0 {
            break
        }
    }
    println!("sorting {} results...", all_guesses.len());

    answers.sort_by(|(a, a_answer, _), (b, b_answer, _)| {
        if a == b {
            a_answer.partial_cmp(b_answer).unwrap()
        } else {
            a.partial_cmp(b).unwrap()
        }
    });

    for (bits, possible, answer) in answers {
        if possible {
            println!("{} (possible answer!) {} bits ", answer, bits);
        } else {
            println!("{} - {} bits ", answer, bits);
        }
    }
}
