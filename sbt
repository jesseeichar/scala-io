java -Dsbt.boot.properties=sbt.boot.properties  -Xms512M -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=384M -jar `dirname $0`/sbt-launch-0.12-RC4.jar "$@"
