import static org.assertj.core.api.Assertions.assertThat;

import org.junit.Test;
import org.junit.Ignore;

public class RaceTrackTest {
    @Ignore("Remove to run test")
    public void car_can_finish_with_car_that_can_easily_finish() {
        int speed = 10;
        int batteryDrain = 2;
        var car = new RemoteControlCar(speed, batteryDrain);

        int distance = 100;
        var race = new RaceTrack(distance);

        assertThat(race.carCanFinish(car)).isTrue();
    }

    @Ignore("Remove to run test")
    @Test
    public void car_can_finish_with_car_that_can_just_finish() {
        int speed = 2;
        int batteryDrain = 10;
        var car = new RemoteControlCar(speed, batteryDrain);

        int distance = 20;
        var race = new RaceTrack(distance);

        assertThat(race.carCanFinish(car)).isTrue();
    }

    @Ignore("Remove to run test")
    @Test
    public void car_can_finish_with_car_that_just_cannot_finish() {
        int speed = 3;
        int batteryDrain = 20;
        var car = new RemoteControlCar(speed, batteryDrain);

        int distance = 16;
        var race = new RaceTrack(distance);

        assertThat(race.carCanFinish(car)).isFalse();
    }

    @Ignore("Remove to run test")
    @Test
    public void car_can_finish_with_car_that_cannot_finish() {
        int speed = 1;
        int batteryDrain = 20;
        var car = new RemoteControlCar(speed, batteryDrain);

        int distance = 678;
        var race = new RaceTrack(distance);

        assertThat(race.carCanFinish(car)).isFalse();
    }
}
