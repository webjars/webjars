web: target/universal/stage/bin/webjars -Dhttp.port=${PORT} -Dplay.filters.enabled.0=play.filters.https.RedirectHttpsFilter
deploy: unset JAVA_TOOL_OPTIONS; target/universal/stage/bin/deploy-web-jar
