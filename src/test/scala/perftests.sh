#!/bin/sh
#export JAVA_OPTS="-Xmx512M -Xms64M -Xprof"
scala -cp ./../../../target/scala-2.9.1/test-classes:./../../../target/scala-2.9.1/classes test.scalafy.PerfTests
#scala -cp ./../../../target/scala-2.9.1/test-classes:./../../../target/scala-2.9.1/classes -J-agentpath:/Applications/YourKit_Java_Profiler_11.0.1.app/bin/mac/libyjpagent.jnilib test.scalafy.PerfTests
