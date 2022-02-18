//
//  localizeTests.swift
//  localizeTests
//
//  Created by eugenen on 18.02.2022.
//

import XCTest

class localizeTests: XCTestCase {
    func testShouldReverseInput() {
        XCTAssertEqual(localize("abcd0"), "0dcba")
    }
}
