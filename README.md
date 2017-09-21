# replchart

Some basic string only charts using ASCII escape sequences and unicode in order
to show graphics in a repl, or maybe in logs.

## Usage

```
(require '[replchart.core :refer [bar-chart mini-bar-chart xy-chart])

replchart.core=> (println (bar-chart [[2 4] [4 8] [5 3] [8 6]]))

8 │
  │
  │
  │
  │
  │
  │
  │

3 └─────────────────────────
  2                        8

nil
replchart.core=> (println (xy-chart [[3 4] [5 3] [4 3] [2 5] [6 2]] :width 60 :height 10))

5 │●
  │
  │
  │               ●
  │
  │
  │
  │                              ●              ●
  │
  │
  │                                                            ●
2 └─────────────────────────────────────────────────────────────
  2                                                            6

nil
replchart.core=> (println (mini-bar-chart (repeatedly 10 rand)))
▃ ▅ ▇ ▅ ▃ ▂ ▄ █ ▁ █

```
## License

Copyright © 2017 Jeff Rose

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
