//
//  localize.swift
//  localize
//
//  Created by eugenen on 18.02.2022.
//

import Foundation

extension Character {
    static func flipCase(_ c: Character) -> String {
        c.isLowercase ? c.uppercased() : c.lowercased()
    }

    var isValidPHPPlaceholderChar: Bool {
        (self >= "a" && self <= "z") || (self >= "A" && self <= "Z") || self == "_"
    }
}

enum CharState {
    case expectsEscapingCompletion
    case expectsPHPPlaceholderCompletion
    case expectsRegularChar
}

enum Token {
    /// A regular character from the input string; its case is flipped.
    case char(Character)
    /// A string as a single token to represent escaped characters in JSON,
    /// such as `\r` and `\"`, or localization placeholders; it stays the
    /// same in the output.
    case string(String)
}

extension Token {
    static func flipCase(_ token: Token) -> String {
        switch token {
        case .char(let char): return Character.flipCase(char)
        case .string(let s): return s
        }
    }
}

extension String {
    func reverseTokens() -> [Token] {
        let initialState = (tokens: [Token](), token: "", charState: CharState.expectsRegularChar)

        /// A list of atoms that are reversed; the atom itself is immutable.
        let state = self.reduce(initialState) { acc, x in
            switch (acc.charState, x) {
            case (CharState.expectsEscapingCompletion, _):
                return (acc.tokens + [.string(acc.token + String(x))], "", .expectsRegularChar)

            case (CharState.expectsPHPPlaceholderCompletion, x)
                    where x.isValidPHPPlaceholderChar:
                return (acc.tokens, acc.token + String(x), .expectsPHPPlaceholderCompletion)

            case (_, "\\"):
                return (acc.tokens + [.string(acc.token)], String(x), CharState.expectsEscapingCompletion)

            case (CharState.expectsPHPPlaceholderCompletion, _):
                return (acc.tokens + [.string(acc.token), .char(x)], "", .expectsRegularChar)

            case (CharState.expectsRegularChar, "$"):
                return (acc.tokens, String(x), CharState.expectsPHPPlaceholderCompletion)

            case (CharState.expectsRegularChar, _):
                return (acc.tokens + [.char(x)], "", .expectsRegularChar)
            }
        }
        // add leftovers at the end, e.g. `$foo` at the end of the string
        let tokens = state.tokens + [.string(state.token)]

        return tokens.reversed()
    }
}

/**
 Fake localization of an English string: reverses the string
 and flips the case of all its characters.
 */
public func localize(_ s: String) -> String {
    s.reverseTokens().compactMap(Token.flipCase).joined()
}
