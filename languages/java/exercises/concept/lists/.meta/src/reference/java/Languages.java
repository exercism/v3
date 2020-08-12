import java.util.ArrayList;
import java.util.List;

public class Languages {

    public static List<String> NewList()
    {
        return new ArrayList<>();
    }

    public static List<String> GetExistingLanguages()
    {
        List<String> languages = new ArrayList<>();
        languages.add("Java");
        languages.add("Clojure");
        languages.add("Go");
        return languages;
    }

    public static List<String> AddLanguage(List<String> languages, String language)
    {
        languages.add(language);
        return languages;
    }

    public static int CountLanguages(List<String> languages)
    {
        return languages.size();
    }

    public static String LastLanguage(List<String> languages){
        return languages.get(languages.size()-1);
    }

    public static boolean HasLanguage(List<String> languages, String language)
    {
        return languages.contains(language);
    }

    public static List<String> RemoveLanguage(List<String> languages, String language)
    {
        languages.remove(language);
        return languages;
    }

    public static boolean EnsureUnique(List<String> languages, String language)
    {
        int count = 0;
        for(String s : languages){
            if (s.equals(language)){
                count = count + 1;
            }
        }
        return count == 1;
    }

    public static int PositionOf(List<String> languages, String language)
    {
        return languages.indexOf(language);
    }
}
