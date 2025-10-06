import { JupyterFrontEnd, JupyterFrontEndPlugin } from '@jupyterlab/application';
import { Widget } from '@lumino/widgets';

/**
 * Replace the Lab logo with your own by adding a widget to the top area.
 * We disabled the core logo plugin, so this becomes the only logo.
 */
const plugin: JupyterFrontEndPlugin<void> = {
  id: 'lab-branding:logo',
  autoStart: true,
  activate: (app: JupyterFrontEnd) => {
    const w = new Widget();
    w.id = 'brand-Logo';
    // Keep it small; adjust height as you like.
    w.node.innerHTML =
      '<img src="/hub/static/branding/lab-logo.svg" alt="Brand" ' +
      'style="height:24px;width:auto;display:block;vertical-align:middle;">';

    // Put it in the TOP area with a low rank => left of the spacer/menu.
    app.shell.add(w, 'top', { rank: 10 });
  }
};

export default plugin;
