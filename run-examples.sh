cabal install --overwrite-policy=always
clear

echo "-----Help-----"
MiniCheck --help

echo "\n-----Validate (Valid)-----"
MiniCheck validate ./examples/vending-machine.txt

echo "\n-----Validate (Invalid)-----"
MiniCheck validate ./examples/invalid.txt

echo "\n-----CTL (Valid)-----"
MiniCheck ctl ./examples/vending-machine.txt "E (F soda)"

echo "\n-----CTL (Invalid)-----"
MiniCheck ctl ./examples/vending-machine.txt "A (F soda)"

echo "\n-----LTL (Valid)-----"
MiniCheck ltl ./examples/vending-machine.txt "(F (beer || soda))" 42

echo "\n-----LTL (Invalid)-----"
MiniCheck ltl ./examples/vending-machine.txt "(F beer)" 42

# Make the script return successfully
true
