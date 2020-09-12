import static org.assertj.core.api.Assertions.assertThat;

import org.junit.Test;
import org.junit.Ignore;

public class RemoteControlCarTest {
    @Test
    public void new_remote_control_car_has_not_driven_any_distance() {
        int speed = 10;
        int batteryDrain = 2;
        var car = new RemoteControlCar(speed, batteryDrain);

        assertThat(car.distanceDriven()).isEqualTo(0);
    }

    @Ignore("Remove to run test")
    @Test
    public void drive_increases_distance_driven_with_speed() {
        int speed = 5;
        int batteryDrain = 1;
        var car = new RemoteControlCar(speed, batteryDrain);

        car.drive();

        assertThat(car.distanceDriven()).isEqualTo(5);
    }

    @Ignore("Remove to run test")
    @Test
    public void drive_does_not_increase_distance_driven_when_battery_drained() {
        int speed = 9;
        int batteryDrain = 50;
        var car = new RemoteControlCar(speed, batteryDrain);

        // Drain the battery
        car.drive();
        car.drive();

        // One extra drive attempt (should not succeed)
        car.drive();

        assertThat(car.distanceDriven()).isEqualTo(18);
    }

    @Ignore("Remove to run test")
    @Test
    public void new_remote_control_car_battery_is_not_drained() {
        int speed = 15;
        int batteryDrain = 3;
        var car = new RemoteControlCar(speed, batteryDrain);

        assertThat(car.batteryDrained()).isFalse();
    }

    @Ignore("Remove to run test")
    @Test
    public void drive_to_almost_drain_battery() {
        int speed = 2;
        int batteryDrain = 1;
        var car = new RemoteControlCar(speed, batteryDrain);

        // Almost drain the battery
        for (var i = 0; i < 99; i++) {
            car.drive();
        }

        assertThat(car.batteryDrained()).isFalse();
    }

    @Ignore("Remove to run test")
    @Test
    public void drive_until_battery_is_drained() {
        int speed = 2;
        int batteryDrain = 1;
        var car = new RemoteControlCar(speed, batteryDrain);

        // Drain the battery
        for (var i = 0; i < 100; i++) {
            car.drive();
        }

        assertThat(car.batteryDrained()).isTrue();
    }

    @Ignore("Remove to run test")
    @Test
    public void top_of_the_line_car_has_not_driven_any_distance() {
        var car = RemoteControlCar.topOfTheLine();
        assertThat(car.distanceDriven()).isEqualTo(0);
    }

    @Ignore("Remove to run test")
    @Test
    public void top_of_the_line_car_has_battery_not_drained() {
        var car = RemoteControlCar.topOfTheLine();
        assertThat(car.batteryDrained()).isFalse();
    }

    @Ignore("Remove to run test")
    @Test
    public void top_of_the_line_car_has_correct_speed() {
        var car = RemoteControlCar.topOfTheLine();
        car.drive();
        assertThat(car.distanceDriven()).isEqualTo(50);
    }

    @Ignore("Remove to run test")
    @Test
    public void top_of_the_line_has_correct_battery_drain() {
        var car = RemoteControlCar.topOfTheLine();

        // The battery is almost drained
        for (var i = 0; i < 24; i++) {
            car.drive();
        }

        assertThat(car.batteryDrained()).isFalse();

        // Drain the battery
        car.drive();

        assertThat(car.batteryDrained()).isTrue();
    }
}

