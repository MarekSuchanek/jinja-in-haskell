import json
from jinja2 import Template

def render_jinja(template_str: str, context_json: str) -> str:
    ctx = json.loads(context_json)
    return Template(template_str).render(ctx)
