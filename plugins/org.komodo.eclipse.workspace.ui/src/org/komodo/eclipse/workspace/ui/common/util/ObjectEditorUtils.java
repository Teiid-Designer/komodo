package org.komodo.eclipse.workspace.ui.common.util;

import org.komodo.relational.model.AbstractProcedure;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.RelationalObject;
import org.komodo.relational.model.Table;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.repository.KomodoObject;

public class ObjectEditorUtils {

	public static boolean setDescription(KomodoObject ko, String desc) {
		try {
			if( ko instanceof Table ) {
				((Table)ko).setDescription(null, desc);
				return true;
			} else if( ko instanceof AbstractProcedure ) {
				((AbstractProcedure)ko).setDescription(null, desc);
				return true;
			} 
//			else if( ko instanceof Parameter ) {
//				((Parameter)ko).setDescription(null, desc);
//				return true;
//			} 
			else if( ko instanceof Column ) {
				((Column)ko).setDescription(null, desc);
				return true;
			} else if( ko instanceof Vdb ) {
				((Vdb)ko).setDescription(null, desc);
				return true;
			} 
//			else if( ko instanceof Model ) {
//				((Model)ko).setDescription(null, desc);
//				return true;
//			} 
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return false;
	}
	
	public static String getDescription(KomodoObject ko) {
		try {
			if( ko instanceof Table ) {
				return ((Table)ko).getDescription(null);
			} else if( ko instanceof AbstractProcedure ) {
				return ((AbstractProcedure)ko).getDescription(null);
			} 
//			else if( ko instanceof Parameter ) {
//				((Parameter)ko).setDescription(null, desc);
//				return true;
//			} 
			else if( ko instanceof Column ) {
				return ((Column)ko).getDescription(null);
			} else if( ko instanceof Vdb ) {
				return ((Vdb)ko).getDescription(null);
			} 
//			else if( ko instanceof Model ) {
//				return ((Model)ko).getDescription(null);
//			} 
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return null;
	}

}
