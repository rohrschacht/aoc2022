import java.io.File
import kotlin.math.floor

class Monkey {
    val monkeyId: Int
    var items: MutableList<Int>
    val operation: ((Int) -> Int)
    val divisor: Int
    val trueTarget: Int
    val falseTarget: Int
    private var inspectedItems: Int = 0

    constructor(
        monkeyId: String,
        startingItems: String,
        operation: String,
        divisor: String,
        trueTarget: String,
        falseTarget: String
    ) {
        this.monkeyId = monkeyId.toInt()
        this.items = startingItems.split(Regex("\\s*,\\s*")).map { item: String -> item.toInt() }.toMutableList()
        this.operation = fun(old: Int): Int {
            var result = 0
            val token = operation.split(" ")
            var leftHandSide = 0
            var rightHandSide = 0
            if (token[0].equals("old")) {
                leftHandSide = old
            } else {
                leftHandSide = token[0].toInt()
            }
            if (token[2].equals("old")) {
                rightHandSide = old
            } else {
                rightHandSide = token[2].toInt()
            }
            if (token[1].equals("+")) {
                result = leftHandSide + rightHandSide
            }
            if (token[1].equals("-")) {
                result = leftHandSide - rightHandSide
            }
            if (token[1].equals("*")) {
                result = leftHandSide * rightHandSide
            }
            if (token[1].equals("/")) {
                result = leftHandSide / rightHandSide
            }
            return result
        }
        this.divisor = divisor.toInt()
        this.trueTarget = trueTarget.toInt()
        this.falseTarget = falseTarget.toInt()
    }

    fun getInspectedItems(): Int {
        return this.inspectedItems
    }

    fun turn(monkeyMap: Map<Int, Monkey>) {
        for (item in this.items) {
            this.inspectedItems++
            val newValue = floor(this.operation(item) / 3.0).toInt()
            if (newValue % this.divisor == 0) {
                monkeyMap[this.trueTarget]!!.items.add(newValue)
            } else {
                monkeyMap[this.falseTarget]!!.items.add(newValue)
            }
        }
        this.items = mutableListOf()
    }
}

fun main() {
    val input = File("./input.txt").readText()
    val monkeyPattern = Regex("Monkey (\\d+):\\n\\s*Starting items: (.*)\\n\\s*Operation: new = (.*)\\n\\s*Test: divisible by (\\d+)\\n\\s*If true: throw to monkey (\\d+)\\n\\s*If false: throw to monkey (\\d+)")
    var monkeys = mutableMapOf<Int, Monkey>()
    for (matchResult in monkeyPattern.findAll(input)) {
        val monkeyId = matchResult.groupValues[1]
        val startingItems = matchResult.groupValues[2]
        val operation = matchResult.groupValues[3]
        val divisor = matchResult.groupValues[4]
        val true_monkey = matchResult.groupValues[5]
        val false_monkey = matchResult.groupValues[6]
        var monkey = Monkey(monkeyId, startingItems, operation, divisor, true_monkey, false_monkey)
        monkeys[monkey.monkeyId] = monkey
    }
    for (roundIndex in 0 until 20) {
        for (monkeyIndex in 0 until monkeys.size) {
            monkeys[monkeyIndex]!!.turn(monkeys)
        }
    }
    var mostInspectedItems = 0
    var secondMostInspectedItems = 0
    for (monkeyIndex in 0 until monkeys.size) {
        val inspectedItems = monkeys[monkeyIndex]!!.getInspectedItems()
        if (inspectedItems > mostInspectedItems) {
            secondMostInspectedItems = mostInspectedItems
            mostInspectedItems = inspectedItems
        } else if (inspectedItems > secondMostInspectedItems) {
            secondMostInspectedItems = inspectedItems
        }
    }
    println(mostInspectedItems * secondMostInspectedItems)
}
