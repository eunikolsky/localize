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
}

enum CharState {
    case expectsEscapingCompletion
    case expectsRegularChar
}

enum Token {
    /// A regular character from the input string; its case is flipped.
    case char(Character)
    /// A string as a single token to represent escaped character in JSON,
    /// such as `\r` and `\"`; it stays the same in the output.
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
        let tokens = self.reduce(initialState) { acc, x in
            switch (acc.charState, x) {
            case (CharState.expectsEscapingCompletion, _):
                return (acc.tokens + [.string(acc.token + String(x))], "", .expectsRegularChar)

            case (CharState.expectsRegularChar, "\\"):
                return (acc.tokens, String(x), CharState.expectsEscapingCompletion)

            case (CharState.expectsRegularChar, _):
                return (acc.tokens + [.char(x)], "", .expectsRegularChar)
            }
        }.tokens

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
