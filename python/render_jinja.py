import json
import sys

import jinja2
import jinja2.sandbox


if __name__ == '__main__':
    # Get input data from stdin (JSON format)
    input_data = sys.stdin.read()
    try:
        data = json.loads(input_data)
    except json.JSONDecodeError as e:
        print(f'Invalid JSON input: {e}',
              file=sys.stderr)
        sys.exit(1)

    # Check if data is a dictionary
    if not isinstance(data, dict):
        print('Input data must be a JSON object (dictionary).',
              file=sys.stderr)
        sys.exit(1)

    input_template = data.get('template', None)
    input_context = data.get('context', None)
    if input_template is None or input_context is None:
        print('Input JSON data must contain "template" and "context" keys.',
              file=sys.stderr)
        sys.exit(1)
    template_str = input_template or ''
    context = input_context or {}

    # Create a Jinja2 environment (use sandbox for security)
    env = jinja2.sandbox.SandboxedEnvironment(
        loader=jinja2.BaseLoader(),
        autoescape=jinja2.select_autoescape(['html', 'xml']),
        extensions=[
            'jinja2.ext.do',
            'jinja2.ext.loopcontrols',
        ]
    )
    # TODO: Add any additional filters or globals if needed

    try:
        # Create a template from the string
        jinja_template = env.from_string(template_str)

        # Render template with input data
        result = jinja_template.render(**context)

        # Print the rendered result
        print(result)

        # Done, exit gracefully
        sys.exit(0)
    except jinja2.TemplateError as e:
        print(f'Template rendering error: {e}',
              file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f'Unexpected error: {e}',
              file=sys.stderr)
        sys.exit(1)
