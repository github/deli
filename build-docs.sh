#!/usr/bin/env bash

docs="$(pandoc -f markdown+lhs -t markdown-fenced_code_attributes app/Tutorial.lhs)"
header="$(printf '%s\n%s\n%s' 'This file is generated, please edit [app/Tutorial.lhs](../app/Tutorial.lhs) instead.' '***' "$docs")"
printf "%s" "$header" > docs/user-guide.md
