package org.komodo.eclipse.workspace.ui.explorer;

import java.util.Collections;

import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;

public class WorkspaceTreeViewer  extends TreeViewer {

//  private EObject selectedDatatype;

  /**
   * Construct an instance of DatatypeHierarchyTreeViewer.
   * @param parent
   */
  public WorkspaceTreeViewer(Composite parent) {
      this(parent, SWT.MULTI);
  }

  public WorkspaceTreeViewer(Composite parent, int style) {
      super(parent, style | SWT.H_SCROLL | SWT.V_SCROLL);
      setContentProvider(new WorkspaceContentProvider());
      setLabelProvider( new WorkspaceLabelProvider());
      super.setInput(Collections.EMPTY_LIST);
      Object item1 = getTree().getItem(0);
      if( item1 != null ) {
    	  
      }
  }

}