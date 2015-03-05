package org.komodo.eclipse.workspace.ui.error;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.IMessage;

public final class ErrorMessage implements IMessage {

    /**
     * The error message (can be <code>null</code> or empty)
     */
    private String message;

    /**
     * The message type.
     */
    private int messageType = IMessageProvider.NONE;

    /**
     * The UI control where the error can be fixed.
     */
    private Control widget;

    /**
     * Clears the error message.
     */
    public void clearMessage() {
        setMessage(null);
    }

    /**
     * {@inheritDoc}
     * 
     * @see org.eclipse.ui.forms.IMessage#getControl()
     */
    @Override
    public Control getControl() {
        return this.widget;
    }

    /**
     * {@inheritDoc}
     * 
     * @see org.eclipse.ui.forms.IMessage#getData()
     */
    @Override
    public Object getData() {
        return null;
    }

    /**
     * {@inheritDoc}
     * 
     * @see org.eclipse.ui.forms.IMessage#getKey()
     */
    @Override
    public Object getKey() {
        return this;
    }

    /**
     * {@inheritDoc}
     * 
     * @see org.eclipse.jface.dialogs.IMessageProvider#getMessage()
     */
    @Override
    public String getMessage() {
        return this.message;
    }

    /**
     * {@inheritDoc}
     * 
     * @see org.eclipse.jface.dialogs.IMessageProvider#getMessageType()
     */
    @Override
    public int getMessageType() {
        return this.messageType;
    }

    /**
     * {@inheritDoc}
     * 
     * @see org.eclipse.ui.forms.IMessage#getPrefix()
     */
    @Override
    public String getPrefix() {
        return null;
    }

    /**
     * @return <code>true</code> if the validation status has an error severity
     */
    public boolean isError() {
        return (this.messageType == IMessageProvider.ERROR);
    }

    /**
     * @return <code>true</code> if the validation status has an information severity
     */
    public boolean isInfo() {
        return (this.messageType == IMessageProvider.INFORMATION);
    }

    /**
     * @return <code>true</code> if the validation status has an OK severity
     */
    public boolean isOk() {
        return (this.messageType == IMessageProvider.NONE);
    }

    /**
     * @return <code>true</code> if the validation status has a warning severity
     */
    public boolean isWarning() {
        return (this.messageType == IMessageProvider.WARNING);
    }

    /**
     * @param newControl the new control (can be <code>null</code>)
     */
    public void setControl( Control newControl ) {
        this.widget = newControl;
    }

    /**
     * @param newMessage the new message (can be <code>null</code> or empty)
     */
    public void setMessage( String newMessage ) {
        this.message = newMessage;
        this.messageType = ((this.message == null || this.message.isEmpty()) ? IMessageProvider.NONE : IMessageProvider.ERROR);
    }

    public void setStatus( IStatus status ) {
        this.message = status.getMessage();

        if (status.getSeverity() == IStatus.ERROR) {
            this.messageType = IMessageProvider.ERROR;
        } else if (status.getSeverity() == IStatus.WARNING) {
            this.messageType = IMessageProvider.WARNING;
        } else if (status.getSeverity() == IStatus.INFO) {
            this.messageType = IMessageProvider.INFORMATION;
        } else if (status.getSeverity() == IStatus.OK) {
            this.messageType = IMessageProvider.NONE;
        }
    }

}