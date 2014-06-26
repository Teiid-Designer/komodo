/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.compare;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import org.komodo.relational.constants.RelationalConstants;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.RelationalObject;
import org.komodo.utils.StringUtil;

/**
 * DifferenceGenerator - generates a difference report, with the differences between two RelationalModels
 */
public class DifferenceGenerator implements RelationalConstants {
	
	/**
	 * Compare two Relational Models
	 * @param targetModel the 'target' Model with the desired end state.
	 * @param originalModel the 'original' Model
	 * @return the difference report
	 */
	public static DifferenceReport compare(Model targetModel, Model originalModel) {
		
		DifferenceReport diffReport = new DifferenceReport();
		
		// Get all Primary Objects for the final desired state
		Collection<RelationalObject> targetChildren = filterForPrimaryRefs(targetModel.getAllReferences());
		
		// Get all Primary Objects for the existing state
		Collection<RelationalObject> originalChildren = filterForPrimaryRefs(originalModel.getAllReferences());		
		
		// The targetChildren will either be a create or a replace
		List<RelationalObject> objsToCreate = new ArrayList<RelationalObject>();
		List<RelationalObject> objsToUpdate = new ArrayList<RelationalObject>();
		List<RelationalObject> objsExactMatch = new ArrayList<RelationalObject>();
		
		Iterator<RelationalObject> iter = targetChildren.iterator();
		while(iter.hasNext()) {
			RelationalObject targetObj = iter.next();
			RelationalObject nameTypeParentMatch = getNameTypeParentMatch(originalChildren,targetObj);
			// The Existing Collection has an object with matching name and type
			if(nameTypeParentMatch!=null) {
				// If not an exact match, put in replace list
				if(!nameTypeParentMatch.equals(targetObj)) {
					objsToUpdate.add(targetObj);
				// Exact match, do nothing with it
				} else {
					objsExactMatch.add(targetObj);
				}
			// No existing children with matching name/type - create it.
			} else {
				objsToCreate.add(targetObj);
			}
		}
		
		// Now determine which of the original objects need to be deleted
		List<RelationalObject> objsToDelete = new ArrayList<RelationalObject>();
		iter = originalChildren.iterator();
		while(iter.hasNext()) {
			RelationalObject origObj = iter.next();
			RelationalObject matchObj = getNameTypeParentMatch(targetChildren,origObj);
			// If no name/type match in the target list, then its a delete
			if(matchObj==null) {
				objsToDelete.add(origObj);
			}
		}

		// Set lists on difference report
		diffReport.setObjectsToCreate(objsToCreate);
		diffReport.setObjectsToDelete(objsToDelete);
		diffReport.setObjectsToUpdate(objsToUpdate);
		
		return diffReport;
	}
	
	/**
	 * Find a RelationalReference in the supplied list which matches another RelationalReference
	 * @param refs the list of RelationalReference objects
	 * @param ref a RelationalReference
	 * @return the matching RelationalReference or null if none found
	 */
	private static RelationalObject getNameTypeParentMatch(Collection<RelationalObject> refs, RelationalObject ref) {
		RelationalObject result = null;
		for(RelationalObject listRef : refs) {
			if(nameTypeParentMatch(listRef,ref)) {
				result = listRef;
				break;
			}
		}
		return result;
	}
	
	/*
	 * Determine if the name, type and parent of the supplied objects match
	 */
	private static boolean nameTypeParentMatch(RelationalObject ref1, RelationalObject ref2) {
		if(ref1==null || ref2==null) {
			return false;
		}
		
        // string properties
        if (!StringUtil.valuesAreEqual(ref1.getName(), ref2.getName()) ) {
            return false;
        }
        
        if(ref1.getType() != ref2.getType()) {
        	return false;
        }

        RelationalObject ref1Parent = ref1.getParent();
        RelationalObject ref2Parent = ref2.getParent();
        if(ref1Parent==null && ref2Parent==null) {
        	return true;
        }
        
        if(ref1Parent==null || ref2Parent==null) {
        	return false;
        }
                
        // Parent types
        if(ref1Parent.getType() != ref1Parent.getType()) {
        	return false;
        }
        
        // Consider model parents equal
        if(ref1Parent.getType()==TYPES.MODEL) {
        	return true;
        // Non-model, names must match
        } else if (!StringUtil.valuesAreEqual(ref1Parent.getName(), ref1Parent.getName()) ) {
            return false;
        }
        
        return true;
	}
	
	/*
	 * Make a list of only the 'primary' objects to create
	 */
	private static Collection<RelationalObject> filterForPrimaryRefs(Collection<RelationalObject> allRefs) {
		List<RelationalObject> filteredList = new ArrayList<RelationalObject>();
		for(RelationalObject rRef : allRefs) {
			if(rRef!=null) {
				int refType = rRef.getType();
				if(refType==TYPES.TABLE || refType==TYPES.PROCEDURE || refType==TYPES.VIEW || refType==TYPES.INDEX ) {
					filteredList.add(rRef);
				}
			}
		}
		return filteredList;
	}
	
	
}
