#ifndef JINJA_BRIDGE_H
#define JINJA_BRIDGE_H

#ifdef __cplusplus
extern "C" {
#endif

// Render Jinja template given template and JSON context
const char* render_jinja(const char* template_str, const char* context_json);

#ifdef __cplusplus
}
#endif

#endif
