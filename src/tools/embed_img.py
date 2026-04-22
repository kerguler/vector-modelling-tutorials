import base64
import mimetypes
import pyperclip

def embed_image_md(path):
    mime_type, _ = mimetypes.guess_type(path)
    #
    with open(path, "rb") as f:
        b64 = base64.b64encode(f.read()).decode()
    #
    # md = f"data:{mime_type};base64,{b64}"
    md = f"{b64}"
    pyperclip.copy(md)
    #    
    return "Markdown copied to clipboard!"

# Example usage
embed_image_md("intro_VEClim_tiles_fig1.png")