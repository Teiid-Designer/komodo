package org.komodo.eclipse.workspace.ui.editor.panels.relational.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import org.eclipse.jface.viewers.ColumnViewer;
import org.komodo.eclipse.workspace.ui.common.table.ComboBoxEditingSupport;

public class ColumnDatatypeEditingSupport extends ComboBoxEditingSupport  implements ViewerElementChangedListener {

	private String[] datatypes;
	
	private ViewerElementChangedListener listener;

	/**
	 * @param viewer
	 *            the column viewer
	 */
	public ColumnDatatypeEditingSupport(ColumnViewer viewer) {
		super(viewer);

		Collection<String> unsortedTypes = new ArrayList<String>();
		// TODO:
//		try {
//			unsortedTypes = DatatypeUtilities.getAllDesignTimeTypeNames();
//		} catch (ModelerCoreException e) {
//			UiConstants.Util.log(e);
//		}
		Collection<String> dTypes = new ArrayList<String>();

		String[] sortedStrings = unsortedTypes.toArray(new String[unsortedTypes.size()]);
		Arrays.sort(sortedStrings);
		for (String dType : sortedStrings) {
			dTypes.add(dType);
		}

		datatypes = dTypes.toArray(new String[dTypes.size()]);

	}

	@Override
	protected String getElementValue(Object element) {
		return null;// ((Column) element).getDatatype();  OR // ((Parameter) element).getDatatype();
	}

	@Override
	protected String[] refreshItems(Object element) {
		return datatypes;
	}

	@Override
	protected void setElementValue(Object element, String newValue) {
		// TODO: ((Column) element).setDatatype(newValue);
		// OR ((Parameter) element).setDatatype(newValue);
		notifyChanged(new ElementChangedEvent(element));
	}
	
	
	@Override
	public void notifyChanged(ElementChangedEvent event) {
		if( this.listener != null ) {
			listener.notifyChanged(event);
		}
	}
	
	public void setViewerElementChangedListener(ViewerElementChangedListener listener) {
		this.listener = listener;
	}
}