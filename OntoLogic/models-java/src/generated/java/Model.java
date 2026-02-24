package com.hivemq.generated;
import jakarta.validation.constraints.*;

public class Asset extends Object {
    @NotNull
    private String serial_number;
}

public class Device extends Asset {
    @NotNull
    private String firmware_ver;
}

public class Sensor extends Device {
    @NotNull
    private Integer temperature;
}

public class Gateway extends Device {
    @NotNull
    private Integer max_connections;
}

