web: target/universal/stage/bin/webjars -XX:+PrintFlagsFinal -Dhttp.port=${PORT} -Dplay.filters.enabled.0=play.filters.https.RedirectHttpsFilter -Dplay.filters.enabled.1=play.filters.gzip.GzipFilter -Dplay.filters.enabled.2=play.filters.csrf.CSRFFilter -Dplay.filters.enabled.3=play.filters.headers.SecurityHeadersFilter
deploy: unset JAVA_TOOL_OPTIONS; target/universal/stage/bin/deploy-web-jar
