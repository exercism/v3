rootProject.name = "exercism-kotlin"

// Collect concept exercises
File("exercises/concept").listFiles()?.forEach { dir ->
    val isGradleProject = dir
        .resolve("build.gradle.kts")
        .exists()

    if (isGradleProject) {
        val relativeFile = dir.relativeTo(File("."))
        includeBuild(relativeFile.path)
    }
}
