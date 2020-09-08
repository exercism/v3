public class ExperimentalRemoteControlCar implements RemoteControlCar {
    int distanceTravelled;

    @Override
    public void drive() {
        distanceTravelled += 20;
    }

    @Override
    public int getDistanceTravelled() {
        return distanceTravelled;
    }
}
