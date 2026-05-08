package webjars.views

import webjars.views.partials.{Footer, Icons, Navbar}

object MainLayout:

  def apply(title: String, currentPath: String = "/", extraHead: String = "")(content: String): String =
    s"""<!DOCTYPE html>
<html lang="en" data-bs-theme="auto">
    <head>
        <meta charset="utf-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <title>$title</title>
        <link rel="shortcut icon" href="/favicon.ico" />
        <link rel="icon" type="image/png" href="/assets/logo.png" />

        <!-- Google fonts -->
        <link rel="preconnect" href="https://fonts.googleapis.com">
        <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
        <link href="https://fonts.googleapis.com/css2?family=Inter:ital,opsz,wght@0,14..32,100..900;1,14..32,100..900&display=swap" rel="stylesheet">

        <!-- Styles -->
        <link rel="stylesheet" href="/assets/main.css">
        <link rel="stylesheet" href="/webjars/select2/4.0.13/css/select2.min.css">
        <link rel="stylesheet" href="/webjars/select2-bootstrap-5-theme/1.3.0/dist/select2-bootstrap-5-theme.min.css">
        <link rel="stylesheet" href="/webjars/highlightjs/11.11.1/styles/atom-one-dark.min.css">

        <!-- Scripts -->
        <script src="/webjars/jquery/3.7.1/jquery.min.js"></script>
        <script src="/webjars/bootstrap/5.3.3/dist/js/bootstrap.bundle.min.js"></script>
        <script src="/webjars/select2/4.0.13/js/select2.full.min.js"></script>
        <script src="/webjars/highlightjs/11.11.1/highlight.min.js"></script>
        <script>hljs.highlightAll();</script>
        <script src="/assets/javascripts/color-modes.js"></script>

        <!-- Google tag (gtag.js) -->
        <script async src="https://www.googletagmanager.com/gtag/js?id=G-EEV2YQX882"></script>
        <script>
          window.dataLayer = window.dataLayer || [];
          function gtag(){dataLayer.push(arguments);}
          gtag('js', new Date());
          gtag('config', 'G-EEV2YQX882');
        </script>
        $extraHead
    </head>

    <body>
        ${Icons()}

        <div class="min-vh-100 h-full d-flex flex-column justify-content-between">
            ${Navbar(currentPath)}

            <div class="flex-grow-1">
                $content
            </div>

            ${Footer()}
        </div>
    </body>
</html>"""
