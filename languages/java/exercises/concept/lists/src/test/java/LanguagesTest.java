import org.junit.Test;
import org.junit.Ignore;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.*;

public class LanguagesTest {
    @Test
    public void newListIsEmpty() {
        assertThat(Languages.newList()).isEmpty();
    }

    @Test
    @Ignore
    public void existingList() {
        List<String> expected = new ArrayList<>();
        expected.add("Java");
        expected.add("Clojure");
        expected.add("Go");
        assertThat(Languages.getExistingLanguages()).isEqualTo(expected);
    }

    @Test
    @Ignore
    public void addLanguage() {
        List<String> initial = new ArrayList<>();
        initial.add("JavaScript");
        initial.add("Bash");
        initial.add("Go");

        List<String> expected = new ArrayList<>();
        expected.add("JavaScript");
        expected.add("Bash");
        expected.add("Go");
        expected.add("C#");

        assertThat(Languages.addLanguage(initial, "C#")).isEqualTo(expected);
    }

    @Test
    @Ignore
    public void countLanguage() {
        List<String> list = new ArrayList<>();
        assertThat(Languages.countLanguages(list)).isEqualTo(0);
        list.add("JavaScript");
        list.add("Bash");
        assertThat(Languages.countLanguages(list)).isEqualTo(2);
    }

    @Test
    @Ignore
    public void lastLanguage() {
        List<String> list = new ArrayList<>();
        list.add("JavaScript");
        list.add("Bash");
        assertThat(Languages.lastLanguage(list)).isEqualTo("Bash");
    }


    @Test
    @Ignore
    public void hasLanguage_yes() {
        List<String> list = new ArrayList<>();
        list.add("JavaScript");
        list.add("Bash");
        assertThat(Languages.hasLanguage(list, "Bash")).isTrue();
    }

    @Test
    @Ignore
    public void hasLanguage_no() {
        List<String> list = new ArrayList<>();
        list.add("JavaScript");
        list.add("Bash");
        assertThat(Languages.hasLanguage(list, "Java")).isFalse();
    }

    @Test
    @Ignore
    public void removeLanguage_yes() {
        List<String> initial = new ArrayList<>();
        initial.add("JavaScript");
        initial.add("Bash");
        initial.add("Go");

        List<String> expected = new ArrayList<>();
        expected.add("JavaScript");
        expected.add("Go");

        assertThat(Languages.removeLanguage(initial, "Bash")).isEqualTo(expected);
    }

    @Test
    @Ignore
    public void removeLanguage_no() {
        List<String> initial = new ArrayList<>();
        initial.add("JavaScript");
        initial.add("Bash");
        initial.add("Go");

        List<String> expected = new ArrayList<>();
        expected.add("JavaScript");
        expected.add("Bash");
        expected.add("Go");

        assertThat(Languages.removeLanguage(initial, "Bash")).isNotEqualTo(expected);
    }

    @Test
    @Ignore
    public void ensureUnique_None() {
        List<String> list = new ArrayList<>();
        list.add("JavaScript");
        list.add("Bash");
        list.add("Go");
        assertThat(Languages.ensureUnique(list, "Java")).isFalse();
    }

    @Test
    @Ignore
    public void ensureUnique_Once() {
        List<String> list = new ArrayList<>();
        list.add("JavaScript");
        list.add("Bash");
        list.add("Go");
        assertThat(Languages.ensureUnique(list, "Go")).isTrue();
    }

    @Test
    @Ignore
    public void ensureUnique_Twice() {
        List<String> list = new ArrayList<>();
        list.add("Go");
        list.add("JavaScript");
        list.add("Bash");
        list.add("Go");
        assertThat(Languages.ensureUnique(list, "Go")).isFalse();
    }

    @Test
    @Ignore
    public void positionOf() {
        List<String> list = new ArrayList<>();
        list.add("JavaScript");
        list.add("Bash");
        list.add("Go");
        assertThat(Languages.positionOf(list, "Go")).isEqualTo(2);
    }

    @Test
    @Ignore
    public void positionOf_TwoOccurence() {
        List<String> list = new ArrayList<>();
        list.add("Go");
        list.add("JavaScript");
        list.add("Bash");
        list.add("Go");
        assertThat(Languages.positionOf(list, "Go")).isEqualTo(0);
    }


}
