# Revision history for teletypedungeon

## 0.1.0.0 -- 2023-02-20

* Can traverse. Basic exposit/elaborate commands.


## 0.2.0.0

* Dice rolling
    - can't do `d20`, but can do `2*1d20+(2d20)`.
    - `2*(1d2) == [1,1] or [2,2]`, `2*1d2 == [1,1] or [1,2] or [2,1] or [2,2]`.
    - New type: `NumbersVal` ([Int]), used entirely for dice rolling.
