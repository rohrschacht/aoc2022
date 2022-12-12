import java.io.File
import java.math.BigInteger
import kotlin.math.floor

class Monkey {
    val monkeyId: Int
    var items: MutableList<Long>
    val operation: ((Long) -> Long)
    val divisor: Long
    val trueTarget: Int
    val falseTarget: Int
    private var inspectedItems: Long = 0

    constructor(
        monkeyId: String,
        startingItems: String,
        operation: String,
        divisor: String,
        trueTarget: String,
        falseTarget: String
    ) {
        this.monkeyId = monkeyId.toInt()
        this.items = startingItems.split(Regex("\\s*,\\s*")).map { item: String -> item.toLong() }.toMutableList()
        this.operation = fun(old: Long): Long {
            var result = 0L
            val token = operation.split(" ")
            var leftHandSide = 0L
            var rightHandSide = 0L
            if (token[0].equals("old")) {
                leftHandSide = old
            } else {
                leftHandSide = token[0].toLong()
            }
            if (token[2].equals("old")) {
                rightHandSide = old
            } else {
                rightHandSide = token[2].toLong()
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
        this.divisor = divisor.toLong()
        this.trueTarget = trueTarget.toInt()
        this.falseTarget = falseTarget.toInt()
    }

    fun getInspectedItems(): Long {
        return this.inspectedItems
    }

    fun turn(monkeyMap: Map<Int, Monkey>, combinedDivisor: Long) {
        for (item in this.items) {
            this.inspectedItems++
            val newValue = this.operation(item)
            if (newValue % this.divisor == 0L) {
                monkeyMap[this.trueTarget]!!.items.add(newValue % combinedDivisor)
            } else {
                monkeyMap[this.falseTarget]!!.items.add(newValue % combinedDivisor)
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

    var combinedDivisor = 1L
    for (monkeyIndex in 0 until monkeys.size) {
        combinedDivisor *= monkeys[monkeyIndex]!!.divisor
    }

    for (roundIndex in 0 until 10000) {
        for (monkeyIndex in 0 until monkeys.size) {
            monkeys[monkeyIndex]!!.turn(monkeys, combinedDivisor)
        }
    }

    var mostInspectedItems = 0L
    var secondMostInspectedItems = 0L
    for (monkeyIndex in 0 until monkeys.size) {
        val inspectedItems = monkeys[monkeyIndex]!!.getInspectedItems()
        println("$monkeyIndex: $inspectedItems")
        if (inspectedItems > mostInspectedItems) {
            secondMostInspectedItems = mostInspectedItems
            mostInspectedItems = inspectedItems
        } else if (inspectedItems > secondMostInspectedItems) {
            secondMostInspectedItems = inspectedItems
        }
    }
    println(mostInspectedItems * secondMostInspectedItems)
}
