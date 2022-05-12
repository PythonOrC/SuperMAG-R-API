public class test {

    public Plot getHighestYield(String c) {

        Plot highestPlot = null;

        int highestVal = -1;
        for (int i = 0; i < 4; i++) {

            for (int j = 0; j < 3; j++) {

                if (highestPlot[i][j].getCropType().equalsIgnoreCase(c)
                        && farmPlots[i][j].getCropYield() > highestVal) {

                    highestVal = farmPlots[i][j].getCropYield();

                    highestPlot = farmPlots[i][j];

                }

            }

        }

        return highestPlot;
    }
}