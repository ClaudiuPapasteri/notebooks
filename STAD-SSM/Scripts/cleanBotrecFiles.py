#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# Copyright 2022 Augmented Space Studios SRL
# Copyright 2022 Sabin Serban
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

import struct
import sys
import os
import json

def cleanList(oldList):
    newList = []
    for i in range(len(oldList)-1, 0, -1):
        newList.append(oldList[i])
        if oldList[i]['timeStamp'] < oldList[i-1]['timeStamp']:
            break
        if i-1 == 0:
            newList.append(oldList[i-1])
    newList.reverse()
    return newList

filenames = []
if len(sys.argv) > 1:
    filenames = sys.argv[1:]
else:
    filenames = [f for f in os.listdir('.') if os.path.splitext(f)[1] == ".botrec"]

if len(filenames) == 0:
    print("""---Invalid arguments---
Script expects the relative paths to .botrec files as arguments,
or '*' for all .botrec files in the same folder as the script.

In the latter case, it will process all .botrec files in the current directory.""")

for f in filenames:
    if not f.endswith('.botrec'):
        continue
    with open(str(f), mode="rb") as file:
        print("Processing file " + str(f))

        base = os.path.splitext(os.path.basename(f))[0]
        fileContent = file.read()
        check0 = struct.unpack("4s", fileContent[:4])
        print("...Header check (#RC0): " + str(check0[0].decode() == "#RC0"))

        headerLength = struct.unpack("i", fileContent[4:8])
        headerStringFormat = str(headerLength[0]) + "s"
        header = struct.unpack(headerStringFormat, fileContent[8:8+headerLength[0]])

        print("...Cleaning Demo and Imitation scores.")
        header_data = json.loads(header[0])
        header_data['sessionConfig']['demoMetrics'] = cleanList(header_data['sessionConfig']['demoMetrics'])
        header_data['sessionConfig']['imitationMetrics'] = cleanList(header_data['sessionConfig']['imitationMetrics'])

        header = json.dumps(header_data)

        print("...Writing clean file " + base + "_clean.botrec")
        with open(base + "_clean.botrec", mode="wb") as clean_file:
            clean_file.write(struct.pack("4s", check0[0]))
            clean_file.write(struct.pack("i", len(header)))
            cleanHeaderStringFormat = str(len(header)) + "s"
            clean_file.write(struct.pack(cleanHeaderStringFormat, header.encode('utf-8')))
            clean_file.write(fileContent[8+headerLength[0]:])

print("DONE")
