plugins {
    java
}

dependencies {
    // Jakarta Validation API (The Annotations)
    implementation("jakarta.validation:jakarta.validation-api:3.0.2")
    
    // Reference implementation for runtime checking
    runtimeOnly("org.hibernate.validator:hibernate-validator:8.0.1.Final")
    runtimeOnly("org.glassfish:jakarta.el:4.0.2")
}
