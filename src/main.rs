use std::{collections::HashMap, sync::mpsc::channel, thread};

use nerdle_solver::{PossibilityMatrix, Equation, Possibility, Key};

fn main() {
    if true {
        let workers = 10;
        if true {
            nerdle(workers)
        } else {
            binerdle(workers)
        }
    } else {
        instant()
    }
}

fn instant() {
    let mut game = PossibilityMatrix::blank(8);
    game.set_certain(0, Key::Digit(7));
    game.eliminate(1, Key::Digit(4));
    game.eliminate(2, Key::Digit(8));
    game.eliminate(3, Key::Divide);
    game.eliminate(4, Key::Digit(1));
    game.eliminate(5, Key::Equal);
    game.eliminate(6, Key::Digit(6));
    game.eliminate(7, Key::Minus);
    game.solutions(|e| {
        println!("{}", e);
        true
    })
}

fn nerdle(workers: usize) {
    let game = PossibilityMatrix::blank(8);

    let mut answers = vec![];

    game.solutions(|possible_answer| {
        answers.push([possible_answer.clone()]);
        true
    });

    compute_entropy_parallel(workers, &answers);
}

fn binerdle(workers: usize) {
    let mut game1 = PossibilityMatrix::blank(8);
    let mut game2 = PossibilityMatrix::blank(8);

    let guess1 = Equation::parse("48-32=16").unwrap();
    game1.feed(&guess1, &[
        Possibility::Impossible,
        Possibility::Impossible,
        Possibility::Impossible,
        Possibility::Unknown,
        Possibility::Unknown,
        Possibility::Certain,
        Possibility::Unknown,
        Possibility::Unknown,
    ]);
    game2.feed(&guess1, &[
        Possibility::Impossible,
        Possibility::Unknown,
        Possibility::Impossible,
        Possibility::Unknown,
        Possibility::Unknown,
        Possibility::Certain,
        Possibility::Unknown,
        Possibility::Unknown,
    ]);

    let guess2 = Equation::parse("59+13=72").unwrap();
    game1.feed(&guess2, &[
        Possibility::Unknown,
        Possibility::Impossible,
        Possibility::Certain,
        Possibility::Unknown,
        Possibility::Unknown,
        Possibility::Certain,
        Possibility::Impossible,
        Possibility::Unknown,
    ]);
    game2.feed(&guess2, &[
        Possibility::Impossible,
        Possibility::Impossible,
        Possibility::Unknown,
        Possibility::Impossible,
        Possibility::Unknown,
        Possibility::Certain,
        Possibility::Unknown,
        Possibility::Unknown,
    ]);

    let guess3 = Equation::parse("3*8+6=30").unwrap();
    game1.feed(&guess3, &[
        Possibility::Certain,
        Possibility::Impossible,
        Possibility::Impossible,
        Possibility::Unknown,
        Possibility::Certain,
        Possibility::Certain,
        Possibility::Impossible,
        Possibility::Impossible,
    ]);
    game2.feed(&guess3, &[
        Possibility::Unknown,
        Possibility::Impossible,
        Possibility::Unknown,
        Possibility::Certain,
        Possibility::Impossible,
        Possibility::Certain,
        Possibility::Impossible,
        Possibility::Impossible,
    ]);

    let mut answers1 = vec![];
    game1.solutions(|possible_answer| {
        answers1.push(possible_answer.clone());
        true
    });

    let mut answers2 = vec![];
    game2.solutions(|possible_answer| {
        answers2.push(possible_answer.clone());
        true
    });

    let mut answers = vec![];
    for answer1 in &answers1 {
        for answer2 in &answers2 {
            answers.push([answer1.clone(), answer2.clone()])
        }
    }

    compute_entropy_parallel(workers, &answers);
}
fn compute_entropy_parallel<const N: usize>(workers: usize, answers: &Vec<[Equation; N]>) {

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
                for tuple in &answers {
                    let mut result = 0;
                    for answer in tuple.iter() {
                        if guess.eq(answer) {
                            is_answer = true;
                            break
                        }
                        result <<= 16;
                        result |= answer.compute_hint_id(&guess)
                    }
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

    for (bits, possible, answer) in &answers {
        if *possible {
            println!("{} (possible answer!) {} bits ", answer, bits);
        } else {
            println!("{} - {} bits ", answer, bits);
        }
    }
    println!("{} total answers", answers.len())
}
