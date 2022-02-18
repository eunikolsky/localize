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

    func testShouldPreserveEscaping() {
        XCTAssertEqual(localize(#"\tHello\r\n \"World\"\b\\"#), #"\\\b\"DLROw\" \n\rOLLEh\t"#)
    }
}
