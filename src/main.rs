use nerdle::{PossibilityMatrix, Key};

fn main() {
    let mut game = PossibilityMatrix::blank(8);

    // 12+35=47
    game.set_certain(0, Key::Digit(1));
    game.eliminate_everywhere(Key::Plus);
    game.eliminate_everywhere(Key::Digit(2));
    game.eliminate_everywhere(Key::Digit(5));
    game.eliminate(5, Key::Equal);
    game.eliminate(3, Key::Digit(3));
    game.eliminate(6, Key::Digit(4));
    game.eliminate(7, Key::Digit(7));

    // 10-6/3=8
    game.set_certain(6, Key::Equal);
    game.eliminate_everywhere(Key::Minus);
    game.eliminate_everywhere(Key::Digit(0));
    game.eliminate_everywhere(Key::Digit(6));
    game.eliminate_everywhere(Key::Digit(8));
    game.eliminate(4, Key::Divide);
    game.eliminate(5, Key::Digit(3));

    game.solutions(|s| {
        println!("{}", s);
        true
    });
}
