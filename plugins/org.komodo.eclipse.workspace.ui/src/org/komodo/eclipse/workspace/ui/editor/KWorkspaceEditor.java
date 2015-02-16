package org.komodo.eclipse.workspace.ui.editor;

import java.beans.PropertyChangeEvent;
import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IStatusLineManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.forms.IManagedForm;
import org.eclipse.ui.forms.IMessageManager;
import org.eclipse.ui.forms.editor.SharedHeaderFormEditor;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.repository.RepositoryImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

public class KWorkspaceEditor extends SharedHeaderFormEditor{
	
	/**
	 * The memento key for the index of the selected editor.
	 */
	private static final String SELECTED_PAGE = "SELECTED_PAGE"; //$NON-NLS-1$

	private boolean dirty = false;
	private boolean readOnly = false;

	private IMemento memento;

	private KomodoObject rootNode;

	private KWorkspaceEditorPage kWorkspaceViewerEditorPage;
	private KNavigableEditorPage kNavigableEditorpage;

	private ScrolledForm scrolledForm;
	private final Collection<KWorkspaceEditorPage> pages = new ArrayList<KWorkspaceEditorPage>(2);

	/**
	 * Allow inner classes access to the <code>DocbookEditorPage</code>s.
	 * 
	 * @return the <code>MedEditorPage</code>s (never <code>null</code>)
	 */
	Collection<KWorkspaceEditorPage> accessKWorkspaceEditorPages() {
		return this.pages;
	}

	/**
	 * Allow inner classes access to the outer class.
	 * 
	 * @return the outer class instance
	 */
	KWorkspaceEditor accessThis() {
		return this;
	}
	
	/**
	 * {@inheritDoc}
	 * 
	 * @see org.eclipse.ui.forms.editor.FormEditor#addPages()
	 */
	@Override
	protected void addPages() {
		// NOTE: pages are added in reverse order
		try {

			// add overview editor
			this.kWorkspaceViewerEditorPage = new KWorkspaceViewerEditorPage(this);
			addPage(0, this.kWorkspaceViewerEditorPage);
			this.pages.add(this.kWorkspaceViewerEditorPage);
			
			this.kNavigableEditorpage = new KNavigableEditorPage(this);
			addPage(0, this.kNavigableEditorpage);
			this.pages.add(this.kNavigableEditorpage);

			this.scrolledForm.setText(getPageText(0));

			// restore state
			int selectedPageNum = 0;

			if (this.memento != null) {
				int value = this.memento.getInteger(SELECTED_PAGE);

				if (value != -1) {
					selectedPageNum = value;
				}
			}

			setActivePage(selectedPageNum);
		} catch (Exception e) {
			// this will open a "Could not open editor" page with exception
			// details
			throw new RuntimeException("Error opening Docbook XML Editor", e); //Messages.errorOpeningMedEditor, e);
		}
	}
	
	/**
	 * {@inheritDoc}
	 * 
	 * @see org.eclipse.ui.forms.editor.FormEditor#init(org.eclipse.ui.IEditorSite,
	 *      org.eclipse.ui.IEditorInput)
	 */
	@Override
	public void init(IEditorSite site, IEditorInput input)
			throws PartInitException {
		super.init(site, input);
		
		try {
			createDocTree();
		} catch (Exception e) {
			throw new PartInitException("Error opening starting Komodo Engine", e); //Messages.errorOpeningMedEditor, e);
		}
	}

	@Override
	public void doSave(IProgressMonitor monitor) {
		internalSave(monitor);
	}
	
	private void internalSave(IProgressMonitor progressMonitor) {
		// TODO
	}

	@Override
	public void doSaveAs() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public boolean isSaveAsAllowed() {
		// TODO Auto-generated method stub
		return false;
	}
	

    /**
     * @return the current workspace node being modified by the GUI (never <code>null</code>)
     */
    protected KomodoObject getRootNode() {
        return this.rootNode;
    }

	private void contributeToMenu(IMenuManager menuMgr) {
//		menuMgr.add(this.updateRegisteryAction);
//		menuMgr.add(this.showRegistryViewAction);
		menuMgr.update(true);
	}

	private void contributeToToolBar(IToolBarManager toolBarMgr) {
//		toolBarMgr.add(this.updateRegisteryAction);
//		toolBarMgr.add(this.showRegistryViewAction);
		toolBarMgr.update(true);
	}
	

	private void createActions() {
		
	}
	
	/**
	 * {@inheritDoc}
	 * 
	 * @see org.eclipse.ui.forms.editor.SharedHeaderFormEditor#createHeaderContents(org.eclipse.ui.forms.IManagedForm)
	 */
	@Override
	protected void createHeaderContents(IManagedForm headerForm) {
		this.scrolledForm = headerForm.getForm();
		//this.scrolledForm.setImage(Activator.getDefault().getImage(MED_EDITOR));

		Form form = this.scrolledForm.getForm();
		getToolkit().decorateFormHeading(form);

		createActions();
		contributeToToolBar(form.getToolBarManager());
		contributeToMenu(form.getMenuManager());
	}
	
	private void createDocTree() throws Exception {
		rootNode = CorePlugin.getWorkspaceRoot();
	}
	
	/**
	 * @return the form editor's message manager (never <code>null</code>)
	 */
	IMessageManager getMessageManager() {
		return this.scrolledForm.getMessageManager();
	}

	private IProgressMonitor getProgressMonitor() {
		IStatusLineManager statusLineMgr = getEditorSite().getActionBars().getStatusLineManager();
		return ((statusLineMgr == null) ? null : statusLineMgr.getProgressMonitor());
	}
	

	/**
	 * @return the editor's shell (never <code>null</code>)
	 */
	Shell getShell() {
		return getEditorSite().getShell();
	}


	/**
	 * {@inheritDoc}
	 * 
	 * @see org.eclipse.ui.forms.editor.SharedHeaderFormEditor#isDirty()
	 */
	@Override
	public boolean isDirty() {
		return this.dirty;
	}
}
