package org.komodo.eclipse.workspace.ui.editor;

import java.beans.PropertyChangeEvent;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.forms.IManagedForm;
import org.eclipse.ui.forms.IMessageManager;
import org.eclipse.ui.forms.editor.FormPage;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.internal.forms.MessageManager;
import org.komodo.eclipse.workspace.ui.error.ErrorMessage;
import org.komodo.spi.repository.KomodoObject;

public abstract class KWorkspaceEditorPage extends FormPage {

    protected KWorkspaceEditorPage( KWorkspaceEditor medEditor,
                             String id,
                             String title ) {
        super(medEditor, id, title);
    }

    /**
     * @param body the parent UI control where all other controls should be added (never <code>null</code>)
     * @param toolkit the form toolkit to use when creating form controls (never <code>null</code>)
     */
    protected abstract void createBody( Composite body,
                                        FormToolkit toolkit );

    /**
     * {@inheritDoc}
     * 
     * @see org.eclipse.ui.forms.editor.FormPage#createFormContent(org.eclipse.ui.forms.IManagedForm)
     */
    @Override
    protected final void createFormContent( IManagedForm managedForm ) {
        createBody(managedForm.getForm().getBody(), managedForm.getToolkit());
        setResourceReadOnly(isReadonly());
        updateAllMessages();
    }

    /**
     * @return the current workspace node being modified by the GUI (never <code>null</code>)
     */
    protected KomodoObject getRootNode() {
        return getKWorkspaceEditor().getRootNode();
    }

    /**
     * @return the model extension definition editor that this page belongs to (never <code>null</code>)
     */
    protected KWorkspaceEditor getKWorkspaceEditor() {
        return (KWorkspaceEditor)getEditor();
    }

//    /**
//     * @return the resource being edited (never <code>null</code>)
//     */
//    protected IFile getFile() {
//        return getDocEditor().getFile();
//    }

    protected Shell getShell() {
        return getSite().getShell();
    }

    /**
     * @param e the property change event being handled (never <code>null</code>)
     */
    protected abstract void handlePropertyChanged( PropertyChangeEvent e );

    /**
     * @return <code>true</code> if the editor is readonly
     */
    protected boolean isReadonly() {
        return false; //getDocEditor().isReadOnly();
    }

    /**
     * {@inheritDoc}
     * 
     * @see org.eclipse.ui.forms.editor.FormPage#setFocus()
     */
    @Override
    public void setFocus() {
        super.setFocus();

        // check for null as this method gets called before controls are constructed
        if (getManagedForm() != null) {
            getManagedForm().refresh();
        }
    }

    /**
     * @param readOnly the new readonly state of the editor
     */
    protected abstract void setResourceReadOnly( boolean readOnly );

    protected abstract void updateAllMessages();

    /**
     * @param errorMessage the message being updated in the {@link IMessageManager message manager} (never <code>null</code>)
     */
    protected void updateMessage( ErrorMessage errorMessage ) {
        assert (errorMessage != null) : "errorMessage is null"; //$NON-NLS-1$
        IMessageManager msgMgr = new MessageManager(getManagedForm().getForm()); // TODO:  (DocbookXmlEditor)getEditor()).getMessageManager();

        if (errorMessage.isOk()) {
            if (errorMessage.getControl() == null) {
                msgMgr.removeMessage(errorMessage.getKey());
            } else {
                msgMgr.removeMessage(errorMessage.getKey(), errorMessage.getControl());
            }
        } else {
            if (errorMessage.getControl() == null) {
                msgMgr.addMessage(errorMessage.getKey(), errorMessage.getMessage(), errorMessage.getData(),
                                  errorMessage.getMessageType());
            } else {
                msgMgr.addMessage(errorMessage.getKey(), errorMessage.getMessage(), errorMessage.getData(),
                                  errorMessage.getMessageType(), errorMessage.getControl());
            }
        }
    }

}