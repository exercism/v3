import org.junit.Test;
import org.junit.Ignore;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.*;
import static org.assertj.core.api.Assertions.*;

public class LanguagesTest {
    @Test
    public void NewListIsEmpty(){
        assertThat(Languages.NewList()).isEmpty();
    }

    @Test
    @Ignore
    public void ExistingList(){
        List<String> expected = new ArrayList<>();
        expected.add("Java");
        expected.add("Clojure");
        expected.add("Go");
        assertThat(Languages.GetExistingLanguages()).isEqualTo(expected);
    }

    @Test
    @Ignore
    public void AddLanguage(){
        List<String> initial = new ArrayList<>();
        initial.add("JavaScript");
        initial.add("Bash");
        initial.add("Go");

        List<String> expected = new ArrayList<>();
        expected.add("JavaScript");
        expected.add("Bash");
        expected.add("Go");
        expected.add("C#");

        assertThat(Languages.AddLanguage(initial,"C#")).isEqualTo(expected);
    }

    @Test
    @Ignore
    public void CountLanguage(){
        List<String> list = new ArrayList<>();
        assertEquals(Languages.CountLanguages(list),0);
        list.add("JavaScript");
        list.add("Bash");
        assertThat(Languages.CountLanguages(list)).isEqualTo(2);
    }

    @Test
    @Ignore
    public void LastLanguage(){
        List<String> list = new ArrayList<>();
        list.add("JavaScript");
        list.add("Bash");
        assertThat(Languages.FirstLanguage(list)).isEqualTo("Bash");
    }


    @Test
    @Ignore
    public void HasLanguage_yes(){
        List<String> list = new ArrayList<>();
        list.add("JavaScript");
        list.add("Bash");
        assertThat(Languages.HasLanguage(list,"Bash")).isTrue();
    }

    @Test
    @Ignore
    public void HasLanguage_no(){
        List<String> list = new ArrayList<>();
        list.add("JavaScript");
        list.add("Bash");
        assertThat(Languages.HasLanguage(list,"Java")).isFalse();
    }

    @Test
    @Ignore
    public void RemoveLanguage_yes(){
        List<String> initial = new ArrayList<>();
        initial.add("JavaScript");
        initial.add("Bash");
        initial.add("Go");

        List<String> expected = new ArrayList<>();
        expected.add("JavaScript");
        expected.add("Go");

        assertThat(Languages.RemoveLanguage(initial,"Bash")).isEqualTo(expected);
    }

    @Test
    @Ignore
    public void RemoveLanguage_no(){
        List<String> initial = new ArrayList<>();
        initial.add("JavaScript");
        initial.add("Bash");
        initial.add("Go");

        List<String> expected = new ArrayList<>();
        expected.add("JavaScript");
        expected.add("Bash");
        expected.add("Go");

        assertThat(Languages.RemoveLanguage(initial,"Bash")).isNotEqualTo(expected);
        //assertThat(Languages.RemoveLanguage(initial,"Bash").equals(expected)).isFalse();
    }

    @Test
    @Ignore
    public void EnsureUnique_None() {
        List<String> list = new ArrayList<>();
        list.add("JavaScript");
        list.add("Bash");
        list.add("Go");
        assertThat(Languages.EnsureUnique(list,"Java")).isFalse();
    }

    @Test
    @Ignore
    public void EnsureUnique_Once() {
        List<String> list = new ArrayList<>();
        list.add("JavaScript");
        list.add("Bash");
        list.add("Go");
        assertThat(Languages.EnsureUnique(list, "Go")).isTrue();
    }

    @Test
    @Ignore
    public void EnsureUnique_Twice() {
        List<String> list = new ArrayList<>();
        list.add("Go");
        list.add("JavaScript");
        list.add("Bash");
        list.add("Go");
        assertThat(Languages.EnsureUnique(list,"Go")).isFalse();
    }

    @Test
    @Ignore
    public void PositionOf() {
        List<String> list = new ArrayList<>();
        list.add("JavaScript");
        list.add("Bash");
        list.add("Go");
        assertThat(Languages.PositionOf(list,"Go")).isEqualTo(2);
    }

    @Test
    @Ignore
    public void PositionOf_TwoOccurence() {
        List<String> list = new ArrayList<>();
        list.add("Go");
        list.add("JavaScript");
        list.add("Bash");
        list.add("Go");
        assertThat(Languages.PositionOf(list,"Go")).isEqualTo(0);
    }


}
