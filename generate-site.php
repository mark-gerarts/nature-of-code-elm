<?php

// Quick and dirty script to generate the static site.

// Build for prod:
// $ php generate-site.php site-structure.json

// Build a version viewable locally:
// $ php generate-site.php site-structure.json ''
// $ cd docs && php -S localhost:8080

$site_structure = file_get_contents($argv[1]);
$site_structure = json_decode($site_structure, TRUE);
if (empty($site_structure)) {
    echo "Invalid structure file";
    exit -1;
}

// Github pages requires publishing to /docs.
$destination_dir = __DIR__ . '/docs';

// Github pages adds the project name as the base path, so take that into
// account.
$base_path = $argv[2] ?? $site_structure['basePath'] ?? '';

$template = <<<HTML
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{{ site_title }}</title>
    <link rel="stylesheet" href="{$base_path}/static/css/main.css">
    <script src="https://unpkg.com/elm-canvas@2.2/elm-canvas.js"></script>
    <script src="{$base_path}/static/js/main.min.js?v=01"></script>
</head>
<body>
    <h1>{{ title }}</h1>
    {{ introduction }}
    {{ main_content }}
</body>
</html>
HTML;

generate_homepage($site_structure, $template, $destination_dir);
foreach ($site_structure['content'] as $module) {
    generate_module_page($site_structure['title'], $module, $template, $destination_dir);
    foreach ($module['content'] as $sketch) {
        generate_sketch_page($site_structure['title'], $module, $sketch, $template, $destination_dir);
    }
}

function cleanup(string $destination_dir) {
    // @todo: remove everything (except static) before generating.
}

function generate_homepage(array $site_structure, string $template, string $destination_dir): void {
    $variables = [
        'site_title' => $site_structure['title'],
        'title' => $site_structure['title'],
        'introduction' => '<p>' . $site_structure['introduction'] . '</p>'
    ];

    $list = '<ul>';
    foreach ($site_structure['content'] as $module) {
        $list .=  sprintf(
            '<li><a href="%s">%s</a>%s</li>',
            url([$module['slug']]),
            $module['title'],
            render_sketch_list($module)
        );
    }
    $list .= '</ul>';
    $variables['main_content'] = $list;

    $homepage = render_page($template, $variables);
    $filename = $destination_dir . '/index.html';
    file_put_contents($filename, $homepage);
}

function generate_module_page(string $site_title, array $module, string $template, string $destination_dir): void {
    $variables = [
        'site_title' => $site_title,
        'title' => $site_title,
        'main_content' => render_sketch_list($module),
        'introduction' => sprintf(
            '<h2>%s</h2><a href="%s">« Home</a>',
            $module['title'],
            url()
        )
    ];

    $module_page = render_page($template, $variables);

    $module_directory = $destination_dir . '/' . $module['slug'];
    if (!file_exists($module_directory)) {
        mkdir($module_directory);
    }
    $filename = $module_directory . '/index.html';
    file_put_contents($filename, $module_page);
}

function generate_sketch_page(string $site_title, array $module, array $sketch, string $template, string $destination_dir): void {
    $main_content_template = <<<HTML
<h2>{{ sketch_title }}</h2>
<main></main>
<p>
    <ul>
        <li><a href="https://github.com/mark-gerarts/nature-of-code-elm/blob/master/src/{{ module }}/{{ sketch }}.elm">View sketch source</a></li>
        <li><a href="{{ chapter_url }}">Go to chapter index</a></li>
        <li><a href="{{ base_url }}">Go to homepage</a></li>
    </ul>
</p>
<script>
    Elm.{{ module }}.{{ sketch }}.init({ node: document.querySelector('main') })
</script>
HTML;

    $main_content_variables = [
        'sketch_title' => $sketch['title'],
        'base_url' => url(),
        'chapter_url' => url([$module['slug']]),
        'source_url' => 'https://www.github.com/mark-gerarts/nature-of-code-elm',
        'module' => $module['module'],
        'sketch' => $sketch['module']
    ];

    $variables = [
        'site_title' => $site_title,
        'title' => $site_title,
        'main_content' => render_page($main_content_template, $main_content_variables)
    ];

    $sketch_page = render_page($template, $variables);

    $sketch_directory = $destination_dir . '/' . $module['slug'] . '/' . $sketch['slug'];
    if (!file_exists($sketch_directory)) {
        mkdir($sketch_directory);
    }
    $filename = $sketch_directory . '/index.html';
    file_put_contents($filename, $sketch_page);
}

function render_sketch_list(array $module): string {
    $sketch_list = '<ul>';
    foreach ($module['content'] as $sketch) {
        $sketch_list .= sprintf(
            '<li><a href="%s">%s</a></li>',
            url([$module['slug'], $sketch['slug']]),
            $sketch['title']
        );
    }
    $sketch_list .= '</ul>';

    return $sketch_list;
}

function url(array $parts = []): string {
    global $base_path;

    return $base_path . '/' . implode('/', $parts);
}

function render_page(string $template, array $variables): string {
    $matches = [];
    preg_match_all('/{{ (.*?) }}/', $template, $matches);
    $possible_variables = $matches[1];

    foreach ($possible_variables as $variable_name) {
        $replacements['{{ ' . $variable_name . ' }}'] = $variables[$variable_name] ?? '';
    }

    return str_replace(
        array_keys($replacements),
        array_values($replacements),
        $template
    );
}
