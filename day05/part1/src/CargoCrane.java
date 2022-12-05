import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;
import java.util.regex.Pattern;

public class CargoCrane {
    private Map<String, Stack<Character>> crateStacks;
    private List<CraneInstruction> instructions;

    public CargoCrane(String inputFile) throws IOException {
        this.crateStacks = new HashMap<>();
        this.instructions = new ArrayList<>();
        var stackSpecification = new Stack<String>();

        var reader = new BufferedReader(new FileReader(inputFile));
        var line = reader.readLine();

        // read the specification of the stacks
        while (line != null && !line.equals("")) {
            stackSpecification.push(line);
            line = reader.readLine();
        }

        // read the crane instructions
        line = reader.readLine();
        var instructionPattern = Pattern.compile("move (\\d+) from (\\d+) to (\\d+)");
        while (line != null) {
            var scanner = new Scanner(line);
            var matcher = instructionPattern.matcher(line);
            matcher.find();

            var numCrates = Integer.parseInt(matcher.group(1));
            var fromStack = matcher.group(2);
            var destinationStack = matcher.group(3);
            var craneInstruction = new CraneInstruction(numCrates, fromStack, destinationStack);
            this.instructions.add(craneInstruction);

            line = reader.readLine();
        }

        // parse the specification of the stacks
        var stackSpecifierLine = stackSpecification.pop();
        var stackSpecifierList = stackSpecifierLine.trim().split("\s+");
        for (var stackSpecifier : stackSpecifierList) {
            var cargoStack = new Stack<Character>();
            this.crateStacks.put(stackSpecifier, cargoStack);
        }

        while (!stackSpecification.empty()) {
            var stackSpecificationLine = stackSpecification.pop();
            for (var stackSpecifier : this.crateStacks.keySet()) {
                var stringPosition = stackSpecifierLine.indexOf(stackSpecifier);
                var cargoItem = stackSpecificationLine.charAt(stringPosition);
                if (cargoItem != ' ')
                    this.crateStacks.get(stackSpecifier).push(cargoItem);
            }
        }
    }

    public void executeInstructions() {
        for (var instruction : this.instructions) {
            for (var i = 0; i < instruction.getNumCrates(); i++) {
                var cargoItem = this.crateStacks.get(instruction.getFromStack()).pop();
                this.crateStacks.get(instruction.getDestinationStack()).push(cargoItem);
            }
        }
    }

    public String getTopCrates() {
        StringBuilder result = new StringBuilder();
        for (var crateStack : crateStacks.values()) {
            var cargoItem = crateStack.peek();
            result.append(cargoItem);
        }
        return result.toString();
    }

    public static void main(String[] args) {
        try {
            var cargoCrane = new CargoCrane("./input.txt");
            cargoCrane.executeInstructions();
            System.out.println(cargoCrane.getTopCrates());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
