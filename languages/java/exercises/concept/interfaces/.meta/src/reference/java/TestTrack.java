import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class TestTrack {

    public static double Race(RemoteControlCar car) {
        car.drive();
        return car.getDistanceTravelled();
    }

    public static List<ProductionRemoteControlCar> GetRankedCars(ProductionRemoteControlCar prc1,
                                                                 ProductionRemoteControlCar prc2) {
        List<ProductionRemoteControlCar> rankings = new ArrayList<>();
        rankings.add(prc1);
        rankings.add(prc1);
        Collections.sort(rankings);

        return rankings;
    }
}
