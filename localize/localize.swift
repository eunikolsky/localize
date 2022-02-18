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

extension String {
    func flipCase() -> String {
        self.compactMap(Character.flipCase).joined()
    }
}

/**
 Fake localization of an English string: reverses the string
 and flips the case of all its characters.
 */
public func localize(_ s: String) -> String {
    String(s.flipCase().reversed())
}
