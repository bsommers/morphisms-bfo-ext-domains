plugins {
    java
    application
}

dependencies {
    implementation(project(":models-java"))
}

application {
    mainClass.set("com.ontologic.app.Main")
}
