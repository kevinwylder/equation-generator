import { useEffect, useState } from 'react';
import styled from 'styled-components';

const PageLayout = styled.div`
display: flex;
flex-wrap: wrap;
justify-content: center;
gap: 20px;
width: 100%;
font-family: Arial,sans-serif;
`

type EquationChar = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "0" | "+" | "-" | "*" | "/" | ""
type ColorChar = "c" | "p" | "a" | ""
interface Guess {
    equation: EquationChar[],
    result: ColorChar[],
}

const emptyGuess: () => Guess = () => ({
    equation: ["", "", "", "", "", "", "", ""],
    result: ["", "", "", "", "", "", "", ""],
})

interface PageProps {
    checkCompiles: (equation: string) => string
}
function Page(props: PageProps) {
    const [guesses, setGuesses] = useState<Guess[]>([]);
    const [guess, setGuess] = useState(emptyGuess())
    const finishGuess = () => {
        setGuesses([...guesses, guess])
        setGuess(emptyGuess());
    }
    return <PageLayout>
        <EquationSection checkCompiles={props.checkCompiles} guess={guess} guesses={guesses} setGuess={setGuess} finishGuess={finishGuess} />
    </PageLayout>
}

interface EquationSectionProps {
    guesses: Guess[]
    guess: Guess
    setGuess: (guesses: Guess) => void
    finishGuess: () => void
    checkCompiles: (equation: string) => string
}
function EquationSection({ guesses, guess, setGuess, finishGuess, checkCompiles }: EquationSectionProps) {
    const [col, setCol] = useState(0);
    return <div>
        {
            guesses.map((guess, i) => <GuessRow setCol={(a) => 0} key={i} guess={guesses[i]} />)
        }
        <GuessRow guess={guess} select={col} setCol={setCol} />
        <Keyboard guess={guess} setGuess={setGuess} col={col} setCol={setCol} finishGuess={finishGuess} checkCompiles={checkCompiles} />
    </div>
}

interface GuessRowProps {
    guess: Guess
    select?: number
    setCol: (col: number) => void
}
function GuessRow({ guess, select, setCol }: GuessRowProps) {
    const getChar = (i: number) => {
        return guess.equation[i];
    }
    const getColor = (i: number) => guess.result[i]
    const onCellClick = (i: number) => () => { setCol(i) }
    return <EquationRowStyle>
        {
            [0, 1, 2, 3, 4, 5, 6, 7].map(i => <Cell key={i} selected={i == select} square={true} color={getColor(i)} onClick={onCellClick(i)}> {getChar(i)} </Cell>)
        }
    </EquationRowStyle>
}

const EquationRowStyle = styled.div`
display: flex;
width: 350px; 
justify-content: center;
align-items: center;
margin-bottom: 0.25rem;
margin-top: 0.25rem;
tab-size: 4;
font-size: 18px;
line-height: 28px;
text-size-adjust: 100%;
`

const colors = {
    c: "rgb(57 136 116)",
    p: "rgb(130 4 88)",
    a: "black",
    "": "rgb(152 148 132)"
}

interface CellProps {
    square?: boolean,
    selected?: boolean,
    color?: ColorChar
    disable?: boolean,
}
const Cell = styled.div<CellProps>`
display: flex;
${({ square }) => square ? "width: 2.8rem;" : `
padding-right: .6rem;
padding-left: .6rem;
`}
height: 2.8rem;
font-weight: 700;
font-size: 1.125rem;
line-height: 1.125rem;
color: ${({ disable }) => disable ? "rgb(200 200 200)" : "white"};
border: 1px solid ${({ selected }) => selected ? "black" : ""};
background-color: ${({ color }) => colors[color || ""]};
border-radius: 0.25rem;
justify-content: center;
align-items: center;
margin-left: 0.125rem;
margin-right: 0.125rem;
`

const ErrBox = styled.div`
font-family: courier;
font-size: .8rem;
color: red;
`

interface KeyboardProps {
    guess: Guess,
    setGuess: (guess: Guess) => void,
    col: number,
    setCol: (col: number) => void
    finishGuess: () => void
    checkCompiles: (equation: string) => string
}
function Keyboard({ guess, setGuess, setCol, col, finishGuess, checkCompiles }: KeyboardProps) {
    const eqnErr = checkCompiles(guess.equation.join(""));
    const colorMode = (eqnErr == "") && !guess.equation.reduce((foundBlank, c) => foundBlank || (c == ""), false);
    const guessReady = colorMode && !guess.result.reduce((foundBlank, c) => foundBlank || (c == ""), false);

    const deepCopyGuess = (guess: Guess) => ({
        equation: [...guess.equation],
        result: [...guess.result],
    });

    const pressDigit = (char: EquationChar) => () => {
        let newGuess = deepCopyGuess(guess);
        newGuess.equation[col] = char
        setGuess(newGuess);
        setCol((col + 1) % 8);
    }

    const pressColor = (char: ColorChar) => () => {
        let newGuess = deepCopyGuess(guess);
        newGuess.result[col] = char;
        setGuess(newGuess);
        setCol(Math.min(col + 1, 7));
    }

    const backspace = () => () => {
        let newGuess = deepCopyGuess(guess);
        if (colorMode) {
            newGuess.result[col] = '';
            col = (col + 7) % 8;
            if (col == 7) {
                newGuess.equation[col] = '';
            } else {
                newGuess.result[col] = '';
            }
        } else {
            newGuess.equation[col] = '';
            col = (col + 7) % 8;
            if (col != 7) {
                newGuess.equation[col] = '';
            }
        }
        setGuess(newGuess);
        setCol(col)
    }

    const enter = () => () => {
        if (guessReady) {
            finishGuess();
            setCol(0);
        }
    }

    useEffect(() => {
        const listener = (event: KeyboardEvent) => {
            if (["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "+", "-", "*", "/", "="].indexOf(event.key) != -1) {
                return pressDigit(event.key as unknown as EquationChar)();
            }
            if (["a", "p", "c"].indexOf(event.key) != -1) {
                return pressColor(event.key as unknown as ColorChar)();
            }
            if (event.key == "Backspace") {
                return backspace()()
            }
            if (event.key == "Enter") {
                return enter()()
            }
        }
        document.addEventListener('keydown', listener);
        return () => document.removeEventListener('keydown', listener);
    })

    return <>
        <ErrBox>{eqnErr}</ErrBox>
        <EquationRowStyle>
            {
                colorMode ? <>
                    <Cell color='c' onClick={pressColor("c")}> Correct </Cell>
                    <Cell color='p' onClick={pressColor("p")}> Present </Cell>
                    <Cell color='a' onClick={pressColor("a")}> Absent </Cell>
                </> :
                    (["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"] as EquationChar[]).map(digit =>
                        <Cell key={digit} onClick={pressDigit(digit)}> {digit} </Cell>
                    )
            }
        </EquationRowStyle>
        <EquationRowStyle>
            {
                !colorMode && (["+", "-", "*", "/", "="] as EquationChar[]).map(digit =>
                    <Cell key={digit} onClick={pressDigit(digit)}> {digit} </Cell>
                )
            }
            <Cell onClick={backspace()}> Delete </Cell>
            <Cell onClick={enter()} disable={!guessReady}> Enter </Cell>
        </EquationRowStyle>
    </>
}

interface SolutionSectionProps { }
function SolutionSection() {
    return <div></div>
}

Promise.all([
    import('react-dom/client'),
    import('../pkg').then(lib => lib.default),
]).then(([{ createRoot }, lib]) => {
    const container = document.getElementById('root');
    if (container) {
        const root = createRoot(container);
        root.render(<Page {...lib} />);
    }
})