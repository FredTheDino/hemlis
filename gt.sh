#!/usr/bin/env bash
cargo b && goldentests --overwrite target/debug/hemlis tests/ -- "-- + "
