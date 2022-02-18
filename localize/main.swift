//
//  main.swift
//  localize
//
//  Created by eugenen on 18.02.2022.
//

import Foundation

let input = String(data: try FileHandle.standardInput.readToEnd() ?? Data(),
                   encoding: .utf8)!
let localized = localize(input).data(using: .utf8)!
FileHandle.standardOutput.write(localized)
