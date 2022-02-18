//
//  localizeTests.swift
//  localizeTests
//
//  Created by eugenen on 18.02.2022.
//

import XCTest

class localizeTests: XCTestCase {
    func testShouldReverseAndFlipCase() {
        XCTAssertEqual(localize("aBcD"), "dCbA")
    }

    func testShouldNotChangeNonCasedCharacters() {
        XCTAssertEqual(localize("0_- !?%8"), "8%?! -_0")
    }

    func testShouldPreserveEmptyInput() {
        XCTAssertEqual(localize(""), "")
    }
}
