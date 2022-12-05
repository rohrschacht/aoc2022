public class CraneInstruction {
    private final int numCrates;
    private final String fromStack;
    private final String destinationStack;

    public int getNumCrates() {
        return numCrates;
    }

    public String getFromStack() {
        return fromStack;
    }

    public String getDestinationStack() {
        return destinationStack;
    }

    public CraneInstruction(int numCrates, String fromStack, String destinationStack) {
        this.numCrates = numCrates;
        this.fromStack = fromStack;
        this.destinationStack = destinationStack;
    }
}
