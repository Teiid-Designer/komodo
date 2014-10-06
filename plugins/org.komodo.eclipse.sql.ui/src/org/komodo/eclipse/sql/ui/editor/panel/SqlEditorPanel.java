/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.eclipse.sql.ui.editor.panel;

import java.util.ArrayList;
import java.util.EventObject;
import java.util.Iterator;
import java.util.List;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.jface.text.IUndoManager;
import org.eclipse.jface.text.TextViewer;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ExtendedModifyEvent;
import org.eclipse.swt.custom.ExtendedModifyListener;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.custom.VerifyKeyListener;
import org.eclipse.swt.custom.ViewForm;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.texteditor.DefaultRangeIndicator;
import org.komodo.eclipse.sql.ui.font.ScaledFontManager;
import org.komodo.eclipse.sql.ui.font.TextFontManager;
import org.komodo.eclipse.sql.ui.graphics.ColorManager;
import org.komodo.modeshape.teiid.parser.QueryParser;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.query.sql.lang.ICommand;
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion.Version;


/**
 * @since 8.0
 */
public class SqlEditorPanel extends SashForm
    implements SelectionListener, ISelectionChangedListener, IPropertyChangeListener,
    KeyListener, MouseListener, IMenuListener, StringConstants {

    private final ITeiidVersion teiidVersion = Version.TEIID_8_7.get();

    /** Changes Pending Message */
    private static final String QUERY_CHANGES_PENDING_MESSAGE = "Pending"; //TODO //$NON-NLS-1$

    private ColorManager colorManager;
    private Color currentBkgdColor;
    private Color widgetBkgdColor;
    private IVerticalRuler verticalRuler;
    private SqlTextViewer sqlTextViewer;
    private StyledTextEditor textEditor;
    private IDocument sqlDocument;
    private boolean messageShowing = true;
    private StyledTextEditor messageArea;
    private ViewForm sqlViewForm;

    // -------------------------------------------------------------------------------------------------------------------
    // DEFECT 23230
    // We need to cache the panelSqlText here to maintain the last setText() value
    // -------------------------------------------------------------------------------------------------------------------
    private String panelSqlText = null;

    boolean validateSelected = false;
    boolean hasPendingChanges = false;
    boolean isCompleteRefresh = false;
    private boolean hasUserError = false;
    private boolean isEditable = true;
    private TextFontManager tfmManager;

    private int caretOffset = -1;
    private int caretXPosition = 0;
    private int caretYPosition = 0;

    /** The width of the vertical ruler. */
    private Object eventSource;

    /** The width of the vertical ruler. */
    protected final static int VERTICAL_RULER_WIDTH = 0;

    /** The editor's text listener. */
    // private ITextListener textListener= new TextListener();
    /** List of listeners registered for this panels events */
    private List eventListeners;
    /** List of listeners registered for this panels internal events */
    private List internalEventListeners;

    String savedSql = EMPTY_STRING;
    String currentMessage = EMPTY_STRING;
    private ICommand command;

    private final QueryParser queryParser;

    private final TeiidParser teiidParser;

    /**
     * Constructor.
     * 
     * @param parent Parent of this control
     */
    public SqlEditorPanel( Composite parent) {
        super(parent, SWT.VERTICAL);

        queryParser = new QueryParser(teiidVersion);
        teiidParser = queryParser.getTeiidParser();

        init();
    }

    /**
     * Initialize the panel.
     */
    private void init() {
        colorManager = new ColorManager();
        currentBkgdColor = getDisplay().getSystemColor(SWT.COLOR_WIDGET_BACKGROUND);
        widgetBkgdColor = getDisplay().getSystemColor(SWT.COLOR_WIDGET_BACKGROUND);

        // ... Do NOT create a vertical ruler at this time (4.0, beta1...GA?)
        // This is used to carry line by line marks, like the error and warning decorations
        // used in the eclipse java ide (jdt). We will not have such features for the
        // forseeable future and until then this space is wasted (see also Defect 10366)
        // verticalRuler= new VerticalRuler(VERTICAL_RULER_WIDTH);

        // add message area to ViewForm to get Eclipse view look
        sqlViewForm = new ViewForm(this, SWT.BORDER);
        int styles = SWT.V_SCROLL | SWT.MULTI | SWT.BORDER | SWT.WRAP | SWT.FULL_SELECTION;
        sqlTextViewer = new SqlTextViewer(teiidVersion, sqlViewForm, verticalRuler, styles, colorManager);
        sqlViewForm.setContent(sqlTextViewer.getControl());

        sqlTextViewer.getTextWidget().addVerifyKeyListener(new VerifyKeyListener() {
            @Override
            public void verifyKey( VerifyEvent event ) {
                if ((event.stateMask == SWT.CTRL && event.character == 127)
                    || (event.stateMask == SWT.CTRL && event.character == ' ')) {
                    event.doit = false;
                    sqlTextViewer.showAssistance();
                }
            }
        });

        sqlTextViewer.getTextWidget().addMouseListener(new MouseListener() {
            @Override
            public void mouseDoubleClick( MouseEvent event ) {
                sqlTextViewer.handleDoubleClick();
            }

            @Override
            public void mouseUp( MouseEvent event ) {
            }

            @Override
            public void mouseDown( MouseEvent event ) {
                captureCaretInfo();
            }
        });

        sqlTextViewer.getTextWidget().addExtendedModifyListener(new ExtendedModifyListener() {
            // This method is invoked every time the text changes
            @Override
            public void modifyText( ExtendedModifyEvent event ) {
                fireEditorInternalEvent(SqlEditorInternalEvent.TEXT_CHANGED);
            }
        });

        this.textEditor = new StyledTextEditor(this.sqlTextViewer);
        this.textEditor.setAlwaysAllowPaste(true);
        this.textEditor.addMenuListener(this);

        // sqlDocument.set("SELECT * FROM TABLE");
        sqlDocument = textEditor.getDocument();
        sqlTextViewer.setEditable(true);
        isEditable = true;

        sqlTextViewer.setRangeIndicator(new DefaultRangeIndicator());

        // Set overall grid layout
        GridLayout gridLayout = new GridLayout();
        this.setLayout(gridLayout);
        gridLayout.numColumns = 1;
        GridData gridData = new GridData(GridData.FILL_BOTH);
        this.setLayoutData(gridData);
        gridData.horizontalAlignment = GridData.FILL;
        gridData.verticalAlignment = GridData.FILL;

        // textArea.addExtendedModifyListener(modifyListener);

        gridData.grabExcessHorizontalSpace = true;
        gridData.grabExcessVerticalSpace = true;
        textEditor.setLayoutData(gridData);

        // add message area to ViewForm to get Eclipse view look
        ViewForm msgViewForm = new ViewForm(this, SWT.BORDER);
        int messageAreaStyle = SWT.READ_ONLY | SWT.V_SCROLL | SWT.WRAP;
        messageArea = StyledTextEditor.createReadOnlyEditor(msgViewForm, messageAreaStyle);
        messageArea.setLayoutData(gridData);
        messageArea.setBackground(widgetBkgdColor);
        msgViewForm.setContent(messageArea.getTextWidget());

        sqlTextViewer.getTextWidget().addSelectionListener(this);
        sqlTextViewer.addSelectionChangedListener(this);
        sqlTextViewer.getTextWidget().addKeyListener(this);
        sqlTextViewer.getTextWidget().addMouseListener(this);

        int[] wts = {4, 1};
        setWeights(wts);

        showMessageArea(false);

        // Add Document Listener to for notification of text changes
        sqlDocument.addDocumentListener(new DocumentChangeListener());
    }

    @Override
    public void menuAboutToShow( IMenuManager manager ) {
        manager.add(new Separator());        
    }

    /**
     * Clears the undo/redo history of the SQL text editor.
     * 
     * @since 5.5.3
     */
    public void resetUndoRedoHistory() {
        this.textEditor.resetUndoRedoHistory();
    }

    /**
     * handle preference change. This only responds to change in formatting preference.
     */
    @Override
    public void propertyChange( PropertyChangeEvent e ) {
//        String propStr = e.getProperty();
//        if (propStr != null
//            && (propStr.equals(org.teiid.query.ui.UiConstants.Prefs.START_CLAUSES_ON_NEW_LINE) || 
//                    propStr.equals(org.teiid.query.ui.UiConstants.Prefs.INDENT_CLAUSE_CONTENT))) {
//            if (!this.isDisposed() && this.isVisible()) {
//                setText(getText());
//            }
//        }
    }

    /**
     * get the SQL Text from the panel
     * 
     * @return the SQL text from the panel
     */
    public String getText() {
        return sqlDocument.get();
    }

    public void clear() {
        savedSql = EMPTY_STRING;
        panelSqlText = null;
        refreshWithDisplayComponent();
    }

    /**
     * Sets the SQL Statement on the Panel
     * 
     * @param SQLString the SQL String to set on the panel
     */
    public void setText( final String sql ) {
        this.eventSource = this;

        // Refresh the EditorPanel, using the queryDisplayComponent
        refreshWithDisplayComponent();
    }

    public boolean threadIsNotDisplayThread() {
        return (this.getDisplay() != null && Thread.currentThread() != this.getDisplay().getThread());
    }

    /**
     * Refreshes the Query JTextPane with the contents of the queryDisplayComponent
     */
    private void refreshWithDisplayComponent() {
        hasPendingChanges = false;
        isCompleteRefresh = true;
        // point the font manager to the current editor's text viewer
        if (threadIsNotDisplayThread()) {
            this.getDisplay().asyncExec(new Runnable() {
                @Override
                public void run() {
                    // Set the SQL Text displayed.
                    sqlDocument.set(panelSqlText);

                    setQueryTextBackground();
                }
            });
        } else {


            // Set the SQL Text displayed.
            sqlDocument.set(panelSqlText);

            setQueryTextBackground();
        }
    }

    /**
     * Fire a SqlEditorEvent to the registered listeners, based on the state of SQL displayed
     * (CHANGES_PENDING,CHANGED,PARSABLE,RESOLVABLE,VALIDATABLE, CARET_CHANGED).
     */
    void fireEditorEvent() {
        // Event source is this panel if null
        if (eventSource == null) {
            eventSource = this;
        }

        boolean isParsable = true;
        SqlEditorEvent event = null;
        // Has Pending Changes
        if (hasPendingChanges) {
            event = new SqlEditorEvent(eventSource, SqlEditorEvent.CHANGES_PENDING);
            // Sql changed but not parsable
        } else if (!isParsable) {
            event = new SqlEditorEvent(eventSource, getText(), SqlEditorEvent.CHANGED);
            // Sql changed, parsable but not resolvable
        } else {
            // fire the event
            event = new SqlEditorEvent(eventSource, getCommand(), getText(), SqlEditorEvent.VALIDATABLE);
        }
        notifyEventListeners(event);
    }

    /**
     * Fire a SqlEditorEvent to the registered listeners indicating CARET_CHANGED on displayed SQL
     * 
     * @since 4.2
     */

    private void notifyCaretChanged() {
        notifyEventListeners(new SqlEditorEvent(this, SqlEditorEvent.CARET_CHANGED));
    }

    /**
     * Fire a SqlEditorInternalEvent to the registered listeners, based on the eventType
     * (SqlEditorInternalEvent.TEXT_RESET,TEXT_INSERT,TEXT_REMOVE,READONLY_CHANGED,CARET_CHANGED
     * 
     * @param eventType the type of internal event to fire
     */
    protected void fireEditorInternalEvent( int eventType ) {
        SqlEditorInternalEvent event = new SqlEditorInternalEvent(this, eventType);
        notifyInternalEventListeners(event);
    }

    /**
     * This method will register the listener for all SqlEditorEvents
     * 
     * @param listener the listener to be registered
     */
    public void addEventListener( EventObjectListener listener ) {
        if (eventListeners == null) {
            eventListeners = new ArrayList();
        }
        eventListeners.add(listener);
    }

    /**
     * This method will un-register the listener for all SqlEditorEvents
     * 
     * @param listener the listener to be un-registered
     */
    public void removeEventListener( EventObjectListener listener ) {
        if (eventListeners != null) {
            eventListeners.remove(listener);
        }
    }

    /**
     * This method will notify the registered listeners of a SqlEditorEvent
     */
    private void notifyEventListeners( EventObject event ) {
        if (eventListeners != null) {
            Iterator iterator = eventListeners.iterator();
            while (iterator.hasNext()) {
                EventObjectListener listener = (EventObjectListener)iterator.next();
                if (listener != null) {
                    listener.processEvent(event);
                }
            }
        }
    }

    /**
     * This method will notify the registered listeners of a SqlEditorEvent
     */
    private void notifyInternalEventListeners( EventObject event ) {
        if (internalEventListeners != null) {
            Iterator iterator = internalEventListeners.iterator();
            while (iterator.hasNext()) {
                EventObjectListener listener = (EventObjectListener)iterator.next();
                if (listener != null) {
                    listener.processEvent(event);
                }
            }
        }
    }

    /**
     * Get the Command for the currently displayed SQL
     * 
     * @return the command, null if the query is not both parseable
     */
    public ICommand getCommand() {
        return command;
    }

    public void showMessageArea( final boolean show ) {
        if (!isDisposed()) {
            if (show != messageShowing) {
                if (threadIsNotDisplayThread()) {
                    this.getDisplay().asyncExec(new Runnable() {
                        @Override
                        public void run() {
                            if (show) {
                                setMaximizedControl(null);
                                setMessage(currentMessage);
                            } else {
                                setMaximizedControl(sqlViewForm);
                            }
                        }
                    });
                } else {
                    if (show) {
                        setMaximizedControl(null);
                        setMessage(currentMessage);
                    } else {
                        setMaximizedControl(sqlViewForm);
                    }
                }
                messageShowing = !messageShowing;
            }
            fireEditorInternalEvent(SqlEditorInternalEvent.MESSAGE_VISIBILITY_CHANGED);
        }
    }

    public boolean isMessageAreaVisible() {
        return messageShowing;
    }

    /**
     * Set the Text in the Message for a QueryDisplayComponent.
     * 
     * @param displayComponent the QueryDisplayComponent
     */
    public void setMessage( String messageText ) {
        currentMessage = messageText;
        if (!messageArea.isDisposed()) {
            // point the font manager to the current editor's text viewer
            if (threadIsNotDisplayThread()) {
                this.getDisplay().asyncExec(new Runnable() {
                    @Override
                    public void run() {
                        if (currentMessage != null) {
                            messageArea.setText(currentMessage);
                        } else {
                            messageArea.setText(""); //$NON-NLS-1$
                        }
                    }
                });
            } else {
                if (currentMessage != null) {
                    messageArea.setText(currentMessage);
                } else {
                    messageArea.setText(""); //$NON-NLS-1$
                }
            }
        }
    }

    /**
     * Method to set background color when the query is not parsable or resolvable
     */
    void setQueryTextBackground() {
        StyledText sqlTextArea = sqlTextViewer.getTextWidget();
        if (sqlTextArea != null) {
            Color bkgdColor = widgetBkgdColor;
            if (hasPendingChanges || hasUserError) {
                bkgdColor = colorManager.getColor(ColorManager.BACKGROUND_INVALID);
            } else {
                if (isEditable()) {
                    bkgdColor = colorManager.getColor(ColorManager.BACKGROUND_VALID);
                }
            }
            setSqlTextAreaBackgroundColor(bkgdColor);
            // Need to set the Line Background color to the same here because it may have been
            // Set in the OTHER method
            StyledText styledText = this.sqlTextViewer.getTextWidget();
            int textLength = styledText.getText().length();
            // get start and end lines for entire text
            int wholeStartLine = 0;
            int wholeEndLine = styledText.getLineAtOffset(textLength);
            int nAllLines = wholeEndLine - wholeStartLine + 1;
            styledText.setLineBackground(wholeStartLine, nAllLines, bkgdColor);
        }
    }

    private void setSqlTextAreaBackgroundColor( Color bkgdColor ) {
        StyledText sqlTextArea = sqlTextViewer.getTextWidget();
        if (sqlTextArea != null) {
            if (!currentBkgdColor.getRGB().equals(bkgdColor.getRGB())) {
                currentBkgdColor = bkgdColor;
                sqlTextArea.setBackground(bkgdColor);
            }
        }
    }

    /**
     * Sets whether the displayed query can be edited
     * 
     * @param status true if the query can be edited, false if not
     */
    public void setEditable( final boolean status ) {
        // point the font manager to the current editor's text viewer

        isEditable = status;

        if (threadIsNotDisplayThread()) {
            this.getDisplay().asyncExec(new Runnable() {
                @Override
                public void run() {
                    sqlTextViewer.setEditable(status);
                    setQueryTextBackground();
                }
            });
        } else {
            sqlTextViewer.setEditable(status);
            setQueryTextBackground();
        }

        fireEditorInternalEvent(SqlEditorInternalEvent.READONLY_CHANGED);
    }

    /**
     * Determine whether the displayed query can be edited
     * 
     * @return true if the query can be edited, false if not
     */
    public boolean isEditable() {
        return isEditable;
    }

    /**
     * Method to set whether the editorPanel has errors
     * 
     * @param status true if theres an error, false if not
     */
    public void setHasError( boolean status ) {
        hasUserError = status;
        // Set error background
        setQueryTextBackground();
    }

    /**
     * Method to determine if the panel has pending changes to the query. This just returns the status of the saveButton.
     * 
     * @return true if the query has pending changes, false if not.
     */
    public boolean hasPendingChanges() {
        return hasPendingChanges;
    }

    public int getCaretOffset() {
        return caretOffset;
    }

    public void setCaretOffset( int posn ) {
        // Prevent setting caret outside of current text
        if (posn < 0) {
            posn = 0;
        }
        int textLength = getText().length();
        if (posn > textLength) {
            posn = textLength;
        }
        // set the caret offset
        this.caretOffset = posn;
        sqlTextViewer.getTextWidget().setCaretOffset(posn);
        fireEditorInternalEvent(SqlEditorInternalEvent.CARET_CHANGED);
    }

    void captureCaretInfo() {
        caretOffset = sqlTextViewer.getTextWidget().getCaretOffset();
        caretYPosition = sqlTextViewer.getTextWidget().getLineAtOffset(caretOffset);
        caretXPosition = caretOffset - sqlTextViewer.getTextWidget().getOffsetAtLine(caretYPosition);
        fireEditorInternalEvent(SqlEditorInternalEvent.CARET_CHANGED);

        notifyCaretChanged();
    }

    public TextViewer getTextViewer() {
        return sqlTextViewer;
    }

    public TextFontManager getFontManager() {

        if (tfmManager == null) {
            tfmManager = new TextFontManager(sqlTextViewer, new ScaledFontManager());
        }
        return tfmManager;
    }

    public IUndoManager getUndoManager() {
        return this.textEditor.getUndoManager();
    }

    public static int getVerticalRulerWidth() {
        return VERTICAL_RULER_WIDTH;
    }

    @Override
    public void widgetSelected( SelectionEvent e ) {
        captureCaretInfo();
    }

    @Override
    public void widgetDefaultSelected( SelectionEvent e ) {
        captureCaretInfo();
    }

    @Override
    public void selectionChanged( SelectionChangedEvent e ) {
    }

    @Override
    public void mouseUp( MouseEvent e ) {
        captureCaretInfo();
    }

    @Override
    public void mouseDown( MouseEvent e ) {
    }

    @Override
    public void mouseDoubleClick( MouseEvent e ) {
        captureCaretInfo();
    }

    @Override
    public void keyPressed( KeyEvent e ) {
    }

    @Override
    public void keyReleased( KeyEvent e ) {
        captureCaretInfo();
    }

    // Handler for DocumentEvents -
    // Whenever the document changes, check vs previous SQL unless already pending changes
    class DocumentChangeListener implements IDocumentListener {
        @Override
        public void documentAboutToBeChanged( DocumentEvent event ) {
        }

        @Override
        public void documentChanged( DocumentEvent event ) {
            // --------------------------------------------------------------
            // If this is a validation, fire event regardless of SQL
            // --------------------------------------------------------------
            if (validateSelected || isCompleteRefresh) {
                validateSelected = false;
                isCompleteRefresh = false;
                hasPendingChanges = false;
                savedSql = getText();
                fireEditorInternalEvent(SqlEditorInternalEvent.TEXT_RESET);
                fireEditorEvent();
                // --------------------------------------------------------------
                // Determine whether changes Pending need to be fired
                // --------------------------------------------------------------
            } else if (!hasPendingChanges) {
                // Check whether the new Sql has changed
                String newSql = getText();
                boolean sqlChanged = hasSqlChanged(newSql.trim());
                savedSql = newSql;
                // SqlChanged
                if (sqlChanged) {
                    setHasPendingChanges();
                }
            }
        }

        boolean hasSqlChanged( String newSql ) {
            if (newSql == null && savedSql != null)
                return true;

            if (newSql != null && savedSql == null)
                return true;

            if (newSql == null && savedSql == null)
                return false;

            return newSql.trim().equals(savedSql.trim());
        }
    }

    public int getCaretYPosition() {
        return this.caretYPosition;
    }

    public int getCaretXPosition() {
        return this.caretXPosition;
    }

    public void setHasPendingChanges() {
        hasPendingChanges = true;
        setMessage(QUERY_CHANGES_PENDING_MESSAGE);
        fireEditorInternalEvent(SqlEditorInternalEvent.TEXT_CHANGED);
        fireEditorEvent();
    }
}
