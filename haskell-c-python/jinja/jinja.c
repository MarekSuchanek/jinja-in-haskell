#include <Python.h>
#include <string.h>

const char* render_jinja(const char* template_str, const char* context_json) {
    static char result_buffer[65536];  // adjust size as needed
    result_buffer[0] = '\0';

    Py_Initialize();

    // Ensure current directory is on sys.path
    PyRun_SimpleString("import sys; sys.path.insert(0, '.')");

    PyObject *module_name = PyUnicode_FromString("renderer");  // renderer.py
    PyObject *module = PyImport_Import(module_name);
    Py_DECREF(module_name);

    if (!module) {
        PyErr_Print();
        strcpy(result_buffer, "ERROR: Cannot import renderer.py");
        Py_Finalize();
        return result_buffer;
    }

    PyObject *func = PyObject_GetAttrString(module, "render_jinja");
    if (!func || !PyCallable_Check(func)) {
        PyErr_Print();
        strcpy(result_buffer, "ERROR: render_jinja not callable");
        Py_XDECREF(func);
        Py_DECREF(module);
        Py_Finalize();
        return result_buffer;
    }

    PyObject *args = Py_BuildValue("(ss)", template_str, context_json);
    PyObject *result = PyObject_CallObject(func, args);
    Py_DECREF(args);

    if (!result) {
        PyErr_Print();
        strcpy(result_buffer, "ERROR: Exception in render_jinja");
    } else {
        const char *rendered = PyUnicode_AsUTF8(result);
        strncpy(result_buffer, rendered, sizeof(result_buffer) - 1);
        result_buffer[sizeof(result_buffer) - 1] = '\0';
        Py_DECREF(result);
    }

    Py_XDECREF(func);
    Py_DECREF(module);
    Py_Finalize();

    return result_buffer;
}
