package org.komodo.eclipse.workspace.ui.editor.panels.relational;

import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import org.komodo.eclipse.workspace.ui.Messages;
import org.komodo.eclipse.workspace.ui.common.util.StyledTextEditor;
import org.komodo.eclipse.workspace.ui.common.util.UILabelUtil;
import org.komodo.eclipse.workspace.ui.common.util.UiLabelConstants;
import org.komodo.eclipse.workspace.ui.common.util.WidgetFactory;
import org.komodo.eclipse.workspace.ui.common.widget.Label;
import org.komodo.eclipse.workspace.ui.editor.panels.IKObjectChangeListener;
import org.komodo.eclipse.workspace.ui.editor.panels.ObjectEditorPanel;
import org.komodo.spi.KException;

public class ModelEditorPanel extends ObjectEditorPanel {
	private TabItem generalPropertiesTab;
    private StyledTextEditor descriptionTextEditor;
	
	public ModelEditorPanel(Composite parent, int style, int nodeType, IKObjectChangeListener objectChangeListener) {
		super(parent, style, nodeType, objectChangeListener, true);
	}
	

	
	@SuppressWarnings("unused")
	public void addCustomPanel() {
		TabFolder tabFolder = getTabFolder();
        
		Composite thePanel = createPropertiesPanel(tabFolder);

        this.generalPropertiesTab = new TabItem(tabFolder, SWT.NONE);
        this.generalPropertiesTab.setControl(thePanel);
        this.generalPropertiesTab.setText(Messages.propertiesLabel);
	}
	
	private Composite createPropertiesPanel(Composite parent) {
		Composite thePanel = WidgetFactory.createPanel(parent, SWT.NONE, 2, 3);
		GridLayoutFactory.fillDefaults().numColumns(2).margins(5, 5).applyTo(thePanel);
		GridDataFactory.fillDefaults().grab(true, true).applyTo(thePanel);
		
		// MODEL Properties
		// TODO:  


        final Group descGroup = WidgetFactory.createGroup(thePanel, UILabelUtil.getLabel(UiLabelConstants.LABEL_IDS.DESCRIPTION), GridData.FILL_BOTH, 2);
        descriptionTextEditor = new StyledTextEditor(descGroup, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL | SWT.WRAP | SWT.BORDER);
        GridDataFactory.fillDefaults().grab(true, true).hint(SWT.DEFAULT, 120).minSize(SWT.DEFAULT, 30).applyTo(descriptionTextEditor.getTextWidget());
        descriptionTextEditor.setText(""); //$NON-NLS-1$
        descriptionTextEditor.getTextWidget().addModifyListener(new ModifyListener() {

            @Override
            public void modifyText(ModifyEvent e) {
//                getRelationalReference().setDescription(descriptionTextEditor.getText());
            }
        });

        return thePanel;
	}
	
	@Override
	protected void syncWithTarget() {
		super.syncWithTarget();
		setSynching(true);

		setSynching(false);
	}

}
