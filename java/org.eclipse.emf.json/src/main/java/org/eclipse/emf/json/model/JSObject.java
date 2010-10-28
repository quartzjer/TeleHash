/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package org.eclipse.emf.json.model;

import org.eclipse.emf.common.util.EMap;
import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>JS Object</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.emf.json.model.JSObject#getContents <em>Contents</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.emf.json.model.JsonPackage#getJSObject()
 * @model
 * @generated
 */
public interface JSObject extends EObject {
	/**
	 * Returns the value of the '<em><b>Contents</b></em>' map.
	 * The key is of type {@link java.lang.String},
	 * and the value is of type {@link java.lang.Object},
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Contents</em>' map isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Contents</em>' map.
	 * @see org.eclipse.emf.json.model.JsonPackage#getJSObject_Contents()
	 * @model mapType="org.eclipse.emf.json.model.EStringToAnySimpleTypeMap<org.eclipse.emf.ecore.EString, org.eclipse.emf.json.model.AnySimpleType>"
	 *        annotation="JsonMetadata wildcard='true'"
	 * @generated
	 */
	EMap<String, Object> getContents();

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model dataType="org.eclipse.emf.json.model.AnySimpleType"
	 * @generated
	 */
	Object get(String key);

} // JSObject
